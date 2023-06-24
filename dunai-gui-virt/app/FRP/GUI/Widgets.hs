{-# LANGUAGE TupleSections #-}
module FRP.GUI.Widgets where

import FRP.BearRiver
import Data.Kind (Type)
import Control.Monad.Reader (ReaderT, MonadTrans (lift), MonadReader (..), asks)

import GI.Gtk (new, ManagedPtr, Button (Button), AttrOp (..), on, IsBuilder, GObject, builderGetObject, unsafeCastTo)
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
import Data.MonadicStreamFunction.ReactHandle (reactInit)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Typeable)
import Control.Exception (Exception, throw)
import Data.GI.Base.Signals
import Data.GI.Base.ShortPrelude
import GHC.TypeLits (KnownSymbol)
import GI.Gtk (get)
import Data.Unique
import FRP.Dynamic

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
  :: (IsBuilder a, GObject o, MonadIO m, Exception (String -> BuilderCastException))
  => a
  -> Text
  -> (ManagedPtr o -> o)
  -> m o
castB builder ident gtype =
  liftIO $ do
    o <- builderGetObject builder ident
    case o of
      Just a -> unsafeCastTo gtype a
      Nothing -> throw UnknownIdException $ T.unpack ident

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
signalEN self signal f b = proc _ -> do
  evAndUniq <- signalEN' self signal f b -< ()
  heldUniq <-
    holdDynOn
      (undefined, -1)
      (\(_, uniq) (_, oldUniq) -> uniq /= oldUniq)
    -< fmap (fmap hashUnique) evAndUniq
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

-- >>> :t arr holdDyn
-- arr holdDyn :: (Arrow a, Monad m) => a b (SF m (Event b) (Dynamic b))

------------------------------
-- BASIC HELLO WORLD IN GTK --
------------------------------


printHello :: IO ()
printHello = putStrLn "Hello, World!"

activateApp :: Gtk.Application -> IO ()
activateApp app = do
  w <- new Gtk.ApplicationWindow [ #application := app
                                 , #title := "Haskell Gi - Examples - Hello World"
                                 , #defaultHeight := 200
                                 , #defaultWidth := 200
                                 ]

  bbox <- new Gtk.ButtonBox [ #orientation := Gtk.OrientationHorizontal ]
  #add w bbox

  btn <- new Gtk.Button [ #label := "Hello World!"]
  on btn #clicked printHello
  on btn #clicked $ Gtk.widgetDestroy w
  #add bbox btn

  #showAll w
  print "test"
  return ()

main :: IO ()
main = do
  -- reactHandle <- reactInit $ runReaderS_ foo undefined
  app <- new Gtk.Application [ #applicationId := "haskell-gi.examples.hello-world"
                             , #flags := [ Gio.ApplicationFlagsFlagsNone ]
                             ]
  on app #activate $ activateApp app
  Gio.applicationRun app Nothing
  -- void . forkIO $ do
  --   react reactHandle
  return ()
