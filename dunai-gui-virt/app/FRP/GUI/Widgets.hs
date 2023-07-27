{-# LANGUAGE TupleSections #-}
module FRP.GUI.Widgets where

import FRP.BearRiver
import Data.Kind (Type)
import Control.Monad.Reader (ReaderT, MonadTrans (lift), MonadReader (..), asks)

import GI.Gtk (new, ManagedPtr, AttrOp (..), on, IsBuilder, GObject, builderGetObject, unsafeCastTo, set, get)
import Data.GI.Base.Attributes
    ( AttrOp, AttrOpTag(AttrConstruct) )
import Data.GI.Base.BasicTypes ( ManagedPtr )
import FRP.GUI.AddHandler
import Control.Monad.Trans.MSF (performOnFirstSample, runReaderT, runReaderS_)
import Data.IORef
import Control.Monad (void)
import Data.GI.Base.Constructible ( Constructible(..) )
import qualified GI.Gtk as Gtk
import qualified GI.Gio as Gio
import qualified GI.Gdk as Gdk
import qualified GI.GLib as GLib
import Data.MonadicStreamFunction.ReactHandle (reactInit, react)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Typeable)
import Control.Exception (Exception, throw)
import Data.GI.Base.Signals
import Data.GI.Base.ShortPrelude
import GHC.TypeLits (KnownSymbol)
import Data.Unique
import FRP.Dynamic
import Data.GI.Base.Overloading
import Control.Monad.Fix (MonadFix)

-- class Widget w where
--   -- | The signals the widget can accept
--   -- FIXME: markdown in haddock comments
--   -- Ex. a textbox may accept Append, Delete, Clear, etc
--   data WidgetInput w :: Type

--   -- | The signals the widget can output. These should track
--   -- the changes in the widget such that it renders correctly.
--   -- Dynamics are often preferred, as they can hold both the
--   -- change and the whole state.
--   -- FIXME: markdown in haddock comments
--   -- Ex. a textbox may output a @Dynamic@ containing its
--   -- most recent addition and its current value
--   data WidgetOutput w :: Type


data Env a = Env
  {
    -- mkWidget :: Handler Widget
    bar :: ()
  }

type GUI a = MSF (ReaderT (Env a) IO)

newtype BuilderCastException = UnknownIdException String
  deriving (Show, Typeable)
instance Exception BuilderCastException

castB
  :: (IsBuilder a, GObject o, MonadIO m)
  => a
  -> Text
  -> (ManagedPtr o -> o)
  -> m o
castB builder ident gtype =
  liftIO $ do
    o <- builderGetObject builder ident
    case o of
      Just a -> unsafeCastTo gtype a
      Nothing -> throw $ UnknownIdException $ T.unpack ident

-- | Cast from a builder object using @castB@. Only evaluates once, returning that stored result.
castBSF
  :: (IsBuilder builder, GObject o, MonadIO m, Exception (String -> BuilderCastException))
  => builder
  -> Text
  -> (ManagedPtr o -> o)
  -> SF m () o
castBSF builder ident gtype = performOnFirstSample $ do
    val <- castB builder ident gtype
    pure $ proc _ -> returnA -< val

signalAddHandler
  :: (SignalInfo info, GObject self)
  => self
  -> SignalProxy self info
  -> ((a -> IO b) -> HaskellCallbackType info)
  -> b
  -> IO (AddHandler a)
signalAddHandler self signal f b = do
  (addHandler, fire) <- newAddHandler
  on self signal (f $ \x -> fire x >> return b)
  return addHandler

-- | Create an 'FRP.Bearriver.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy'. For making signalE# functions.
-- Provides the fire method from 'FRP.GUI.AddHandler.newAddHandler'
-- for creating a callback.
-- WARNING: The event emitted from this function will be outputted continuously!
signalEN'
  :: (SignalInfo info, GObject self, MonadIO m)
  => self
  -> SignalProxy self info
  -> ((a -> IO b) -> HaskellCallbackType info)
  -> b
  -> SF m () (Event (a, Unique)) --m (Event a)
signalEN' self signal f b = performOnFirstSample $ do
  addHandler <- liftIO $ signalAddHandler self signal f b

  eventStorage <- liftIO $ newIORef NoEvent --hold a queue of events
  uniqueStorage <- liftIO $ newUnique >>= newIORef --make a new storage for unique IDs, with a dummy new unique val

  pure $ arrM $ \_ -> do
    --Register the event in the event storage and generate a new ID
    --FIXME: this block may need to be moved up, as it may result in duplicate registrations
    liftIO $ register addHandler $ \a -> do
      unique <- liftIO newUnique --generate a new unique
      writeIORef uniqueStorage unique --put the new unique in its storage
      writeIORef eventStorage $ Event a --put the new event in its storage
      pure ()
    --FIXME: do something with unregister? how to make sure event isnt duplicated?
    ev <- liftIO $ readIORef eventStorage
    uniq <- liftIO $ readIORef uniqueStorage
    pure $ fmap (, uniq) ev

signalEN
  :: (SignalInfo info, GObject self, MonadIO m)
  => self
  -> SignalProxy self info
  -> ((a -> IO b) -> HaskellCallbackType info)
  -> b
  -> SF m () (Event a)
signalEN self signal f b = performOnFirstSample $ do
  dummyUnique <- liftIO newUnique
  pure $ proc _ -> do
    evAndUniq <- signalEN' self signal f b -< ()
    heldUniq <-
      holdDynOn
        (undefined, dummyUnique) --placeholder and sentinel value
        (\(_, uniq) (_, oldUniq) -> uniq /= oldUniq)
      -< evAndUniq
    let x = fmap fst heldUniq
    returnA -< updated x

-- | Get an 'Reactive.Banana.Event' from
-- a 'Data.GI.Base.Signals.SignalProxy' that produces nothing.
--
-- @
-- destroyE <- signalE0 window #destroy
-- @
signalE0
  :: (MonadIO m, HaskellCallbackType info ~ IO (), SignalInfo info, GObject self)
  => self
  -> SignalProxy self info
  -> SF m () (Event ())
signalE0 self signal = signalEN self signal ($ ()) ()

signalE1
  :: ( MonadIO m, HaskellCallbackType info ~ (a -> IO ())
     , SignalInfo info
     , GObject self
     , MonadIO m
     )
  => self
  -> SignalProxy self info
  -> SF m () (Event a)
signalE1 self signal = signalEN self signal id ()

signalE0R
  :: ( HaskellCallbackType info ~ IO b
     , SignalInfo info
     , GObject self
     , MonadIO m
     )
  => self
  -> SignalProxy self info
  -> b
  -> SF m () (Event ())
signalE0R self signal = signalEN self signal ($ ())

signalE1R
  :: ( HaskellCallbackType info ~ (a -> IO b)
     , SignalInfo info
     , GObject self
     , MonadIO m)
  => self
  -> SignalProxy self info
  -> b
  -> SF m () (Event a)
signalE1R self signal = signalEN self signal id

attrE
  :: ( GObject self
     , AttrGetC info self attr result
     , KnownSymbol (AttrLabel info)
     , MonadIO m
     )
  => self
  -> AttrLabelProxy (attr :: Symbol)
  -> SF m () (Event result) --m (Event result)
attrE self attr = proc _ -> do
  e <- signalE1 self (PropertyNotify attr) -< ()
  retAttr <- arrM $ get self -< attr --get the current attribute's value
  returnA -< tag e retAttr --return the attr given the event fired

-- | Track an attribute
attrD
  :: ( GObject self
     , AttrGetC info self attr result
     , KnownSymbol (AttrLabel info)
     , MonadIO m
     )
  => self
  -> AttrLabelProxy (attr :: Symbol)
  -> SF m () (Dynamic result)
attrD self attr = performOnFirstSample $ do
  initValue <- get self attr
  pure $ proc _ -> do
    ev <- attrE self attr -< ()
    holdDyn initValue -< ev --update the dynamic with the new value

-- attrD'
--   :: ( GObject self
--      , AttrGetC info self attr result
--      , KnownSymbol (AttrLabel info)
--      , MonadIO m
--      )
--   => AttrLabelProxy (attr :: Symbol)
--   -> SF m self (Dynamic result)
-- attrD' attr = performOnFirstSample $ do
--   --this function is a bit convoluted, but it is needed(?) to skirt around the
--   --restrictions placed by arrows

--   --empty storage, where the future
--   --value of "self" will be stored
--   initStorage <- liftIO $ newIORef undefined

--   pure $ proc self -> do
--     attrVal <- arrM (`get` attr) -< self --get the attribute value

--     --add the initial attribute value into storage
--     arrM (liftIO . writeIORef initStorage) -< attrVal
--     --TODO: convert attrE into accepting non-static widget
--     holdDynNonStatic (readIORef initStorage) -< ev
--     -- ev <- attrE self attr -< ()
--     -- holdDyn initValue -< ev --update the dynamic with the new value

-- attrE
--   :: ( GObject self
--      , AttrGetC info self attr result
--      , KnownSymbol (AttrLabel info)
--      , MonadIO m
--      )
--   => self
--   -> AttrLabelProxy (attr :: Symbol)
--   -> SF m () (Event result) --m (Event result)
-- attrE self attr = performOnFirstSample $ do
--   selfStorage
--   proc _ -> do
--   e <- signalE1 self (PropertyNotify attr) -< ()
--   retAttr <- arrM $ get self -< attr --get the current attribute's value
--   returnA -< tag e retAttr --return the attr given the event fired

-- TODO: make Button opaque
newtype Button = Button
  {
    gtkButton :: Gtk.Button
  }

-- mkWidget
--   :: (MonadIO m)
--   => (ManagedPtr a -> a)
--   -> [AttrOp a 'AttrConstruct]
--   -> SF m [AttrOp a 'AttrSet] a
-- mkWidget widget initAttrs = performOnFirstSample $ do
--   w <- new widget initAttrs
--   pure $ proc attrs -> do
--     arrM (set w) -< attrs
--     returnA -< w

mkButton
  :: MonadIO m
  => [AttrOp Gtk.Button 'AttrConstruct]
  -> SF m [AttrOp Gtk.Button 'AttrSet] Button
mkButton initAttrs = performOnFirstSample $ do
  button <- new Gtk.Button initAttrs
  pure $ proc attrs -> do
    arrM (set button) -< attrs
    returnA -< Button button

pressed
  :: forall m
   . MonadIO m
  => SF m Button Bool --bool or event?
pressed = performOnFirstSample $ do
  evStorage <- liftIO $ newIORef False
  pure $ proc btn -> do
    switch (initSF evStorage) (newSF evStorage) -< btn
  where
    initSF :: IORef Bool -> SF m Button (Bool, Event Bool)
    initSF evStorage = proc btn -> do
      --the point of initSF is to apply @on btn #clicked@ ONCE and ONLY ONCE
      arrM (\x -> do
              liftIO $
                on (gtkButton x) #clicked
                (writeIORef evStorage True)
              liftIO $ print "in arrM"
           ) -< btn
      returnA -< (False, Event False) --we always want to immediately switch
    newSF :: IORef Bool -> Bool -> SF m Button Bool
    newSF evStorage = const $ proc btn -> do --the event we receive does not matter
      -- status is written to when the button is pressed
      status <- arrM (const $ liftIO $ readIORef evStorage) -< ()
      -- reset the status
      arrM (liftIO . writeIORef evStorage) -< False
      returnA -< status




testWidget :: (MonadIO m, MonadFix m) => Gtk.Application -> SF m () ()
testWidget app = performOnFirstSample $ do
  w <- new Gtk.Window [ #application := app
                      , #title := "Haskell Gi - Examples - Hello World"
                      , #defaultHeight := 200
                      , #defaultWidth := 200
                      ]
  Gtk.applicationAddWindow app w

  bbox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  Gtk.setWindowChild w bbox

  pure $ proc _ -> do
    myButton <-
        mkButton [
                  #label := "This is the yampa one"
                ] -< []
    otherBtn <-
        mkButton [
                  #label := "other btn"
                ] -< []
    rec
      arrM (uncurry set) -< (gtkButton myButton, [ #useUnderline := otherBtnPressed ])
      arrM (uncurry set) -< (gtkButton myButton, [ #useUnderline := pressedEv ])
      pressedEv <- pressed -< myButton
      otherBtnPressed <- pressed -< otherBtn
    arrM $ liftIO . putStr -< if pressedEv then "output from the button!!!!!!\n" else ""

    performSFOnce $ arrM (\(myButton, otherBtn) -> do
              #append bbox (gtkButton myButton)
              #append bbox (gtkButton otherBtn)

              Gtk.widgetSetVisible w True
              return ()
            ) -< (myButton, otherBtn)

-- | Perform a SF once, returning that result afterwards
performSFOnce :: forall m a b. Monad m => SF m a b -> SF m a b
performSFOnce sf = proc a -> do
  switch initSF holdSF -< a
  where
    initSF :: SF m a (b, Event b)
    initSF = proc a -> do
      out <- sf -< a
      returnA -< (out, Event out)
    holdSF :: b -> SF m a b
    holdSF out = proc _ -> do
      returnA -< out


-- >>> :t switch
-- switch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b


data AttrOpSF self tag m inp where
  (:->)
    :: ( HasAttributeList self
       , info ~ ResolveAttribute attr self
       , AttrInfo info
       , AttrBaseTypeConstraint info self
       , AttrOpAllowed tag info self
       , AttrSetTypeConstraint info b
       , MonadIO m
       )
    => AttrLabelProxy (attr :: Symbol)
    -> SF m a b
    -> AttrOpSF self tag m a
infixr 0 :->

-- | Change an attribute of a widget using a SF
-- Example usage:
-- @
-- myLabel
-- sink myLabel
sink
  :: ( GObject self
     , MonadIO m
     )
  => self
  -> AttrOpSF self AttrSet m a
  -> SF m a ()
sink self (attr :-> sf) = proc a -> do
  sfOut <- sf -< a
  -- TODO: see if setting without diffing in sink1 causes
  -- unnecessary recomputation
  arrM (\x -> set self [attr := x]) -< sfOut

-- | Create a widget once using statically known constants
mkWidgetKnown
   :: ( Constructible a tag
      , MonadIO m
      )
   => (ManagedPtr a -> a) -- ^ The widget in question. Example: @GI.Gtk.Button@
   -> [AttrOp a tag]
   -> SF m () a
mkWidgetKnown widget attrs = performOnFirstSample $ do
  w <- new widget attrs
  pure $ proc _ -> do
    returnA -< w

-- | Create a widget using dynamically created attributes. The widget will not
-- react to these attributes. A new widget will be created for every event
-- this SF receives!
mkWidgetStatic
  :: ( Constructible a tag
     , MonadIO m
     )
  => (ManagedPtr a -> a) -- ^ The widget in question. Example: @GI.Gtk.Button@
  -> SF m (Event [AttrOp a tag]) (Event a)
mkWidgetStatic widget = proc attrE -> do
  case attrE of
    -- return a new Event containing the new widget
    Event attrs -> Event <$> arrM (new widget) -< attrs
    -- return nothing
    NoEvent     -> returnA -< NoEvent

-- -- | Create a widget using dynamically created attributes. The widget will not
-- -- react to these attributes. Only one widget will be created.
-- -- FIXME: determine if it does react, and if it flashes due to constant changes
-- mkWidgetOnce
--   :: ( Constructible a tag
--      , MonadIO m
--      )
--   => (ManagedPtr a -> a) -- ^ The widget in question. Example: @GI.Gtk.Button@
--   -> SF m [AttrOp a tag] a
-- mkWidgetOnce widget = performOnFirstSample $ do
--   --FIXME: does this widget flash because of the use of empty attributes?
--   w <- new widget []
--   pure $ proc attrs -> do
--     arrM (set self) -< attrs -- apply the attributes


-- | Create a widget once
mkWidgetReact
  :: ( Constructible a tag
     , MonadIO m
     )
  => (ManagedPtr a -> a) -- ^ The widget in question. Example: @GI.Gtk.Button@
  -> SF m (Event [AttrOp a tag]) (Event a)
mkWidgetReact = undefined




------------------------------
-- BASIC HELLO WORLD IN GTK --
------------------------------


printHello :: IO ()
printHello = putStrLn "Hello, World!"

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  w <- new Gtk.Window [ #application := app
                      , #title := "Haskell Gi - Examples - Hello World"
                      , #defaultHeight := 200
                      , #defaultWidth := 200
                      ]
  Gtk.applicationAddWindow app w

  bbox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal ]
  Gtk.setWindowChild w bbox

  btn <- new Gtk.Button [ #label := "Hello World!"]
  on btn #clicked printHello

  #append bbox btn

  Gtk.widgetSetVisible w True
  return ()

--how does actuate work in reactive banana? does it spawn another thread?
--how should i generate new widgets defined outside of the GUI builder on another thread?
-- ^ add idle in glib

main :: IO ()
main = do
  app <- new Gtk.Application [ #applicationId := "haskell-gi.examples.hello-world"
                             , #flags := [ Gio.ApplicationFlagsFlagsNone ]
                             ]
  reactHandle <- reactInit $ runReaderS_ (testWidget app) 0.1
  on app #activate $ activateApp app

  GLib.idleAdd 1 (react reactHandle >> pure True)
  Gio.applicationRun app Nothing
  -- void . forkIO $ do
  --   react reactHandle
  return ()
