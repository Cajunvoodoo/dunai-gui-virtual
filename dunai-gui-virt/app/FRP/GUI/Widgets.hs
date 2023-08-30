{-# LANGUAGE TupleSections #-}
module FRP.GUI.Widgets where

import qualified Control.Monad.Trans.MSF as MSF
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))
import FRP.BearRiver
import Data.Kind (Type)
import Control.Monad.Reader (ReaderT, MonadTrans (lift), MonadReader (..), asks)
import FRP.GUI.AddHandler
import FRP.GUI.Utils
import Control.Monad.Trans.MSF (performOnFirstSample, runReaderT, runReaderS_)
import Data.IORef
import Control.Monad (void)
import Data.MonadicStreamFunction.ReactHandle (reactInit, react)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Typeable)
import Data.Unique
import FRP.Dynamic
-- import Data.GI.Base.Overloading
import Control.Monad.Fix (MonadFix)
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import SDL hiding (Event)
import Control.Monad.Managed
import Control.Exception
import DearImGui.SDL.OpenGL
import Control.Monad.Identity
import Graphics.GL
import Control.Monad.Trans.MSF.Reader (runReaderS)
import Data.Maybe (fromMaybe)
import Foreign.C (CFloat(CFloat))
import GHC.Float (double2Float)


-- | A widget ID by name
type ID = Text

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged $ do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext


    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    liftIO $ reactimate' @IO (testWidget window)

reactimate' :: MonadIO m => SF IO () () -> IO ()
reactimate' sf =
  reactimate'' (pure ()) sense actuate sfIO
  where
    sfIO = sf
    sense :: Bool -> IO (DTime, Maybe ())
    sense _ = pure (0.2, Nothing)
    actuate _ _ = pure False

reactimate'' :: Monad m => m a -> (Bool -> m (DTime, Maybe a)) -> (Bool -> b -> m Bool) -> SF m a b -> m ()
reactimate'' senseI sense actuate sf = do
  -- runMaybeT $ MSF.reactimate $ liftMSFTrans (senseSF >>> sfIO) >>> actuateSF
  MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
  return ()
 where sfIO        = runReaderS sf

       -- Sense
       senseSF     = MSF.dSwitch senseFirst senseRest

       -- Sense: First sample
       senseFirst = constM senseI >>> arr (\x -> ((0, x), Just x))

       -- Sense: Remaining samples
       senseRest a = constM (sense True) >>> (arr id *** keepLast a)

       keepLast :: Monad m => a -> MSF m (Maybe a) a
       keepLast a = MSF $ \ma -> let a' = fromMaybe a ma in a' `seq` return (a', keepLast a')

       -- Consume/render
       -- actuateSF :: MSF IO b ()
       -- actuateSF    = arr (\x -> (True, x)) >>> liftMSF (lift . uncurry actuate) >>> exitIf
       actuateSF    = arr (\x -> (True, x)) >>> arrM (uncurry actuate)


testWidget :: forall m. (MonadIO m, MonadFix m) => Window -> SF m () ()
testWidget window = proc _ -> do

  constM pollEventWithImGui -< ()

  -- make a new frame
  constM openGL3NewFrame -< ()
  constM sdl2NewFrame    -< ()
  constM newFrame        -< ()

  -- get the current DTime


  mkWindowOpen ( proc _ -> do
                   mkButton btnSF -< "Clickety Click"

                   sinWave <- FRP.BearRiver.time >>> arr (double2Float . sin) -< ()
                   mkPlotLines 100 -< (sinWave, "sin")

                   rec
                     outTextField <- mkTextMultiline <<< iPre (emptyTextField "TF" (ImVec2 500 500)  100)
                        -< outTextField

                   -- rec
                   --   pressed <- mkButton btnSF <<< iPre "" -< "Clickety Click: " <> tshow btnStatus
                   --   btnStatus <- never >>> hold False -< Event btnStatus

                   returnA -< ()
               ) -< ((), "Hello, ImGui!")

  constM showDemoWindow -< ()

  -- Render
  arrM glClear -< GL_COLOR_BUFFER_BIT
  constM render -< ()
  constM (openGL3RenderDrawData =<< getDrawData) -< ()

  arrM glSwapWindow -< window

  where
    btnSF :: SF m Bool Bool
    btnSF = proc b -> do
      if b
        then do
          arrM $ liftIO . print -< "Ow!"
          returnA -< b
        else returnA -< b
    testSF :: Monad n => SF n Bool Bool
    testSF = arr not





-- | Make a button with a provided signal function.
--   This corresponds similarly to the dear-imgui function @button@.
--   @
--   btn <- button "Clickety Click" >>= \case
--     False -> return ()
--     True  -> do
--       putStrLn "Ow!"
--   @
--   Where the supplied SF acts as the continuation based on the button output.
mkButton :: MonadIO m => SF m Bool b -> SF m ID b
mkButton sf = proc name -> do
  btnOut <- arrM button -< name
  sf -< btnOut

-- TODO: variant of @mkWindowOpen@ to enable stuff like @setNextWindowFullscreen@,
-- which requires it to be called before calling @withWindowOpen@
-- TODO: Instead of a tuple, have some sort of record, maybe using Data.Default?
-- | Make a window in which widgets can be drawn in. The provided signal function
--   draws the widgets inside of this window.
mkWindowOpen :: MonadIO m => SF m a b -> SF m (a, ID) b
mkWindowOpen sf = proc (a, name) -> do
  arrM begin -< name     -- BEGIN
  b <- sf -< a -- What to do inside of the widget while it is open
  arrM $ const end -< () -- END
  returnA -< b

-- | Make a plot over time holding @n@ maximum datapoints. The signal function
--  accepts the new datapoint and the name of the graph. When a new datapoint
--  is received, the leftmost is pushed out for the new rightmost datapoint.
mkPlotLines :: MonadIO m => Int -> SF m (Float, ID) ()
mkPlotLines len = proc (n, name) -> do
  list <- holdList len -< Event $ CFloat n
  arrM $ uncurry plotLines -< (name, list)

-- | A text field has 4 fields:
--      * textDyn   :: Dynamic Text
--      * label     :: Text
--      * fieldSize :: ImVec2
--      * buffSize  :: Int
--   All of which are required to properly render a text field.
--   The buff size
data TextField = TextField
  {
    textDyn   :: Dynamic Text
  , label     :: Text
  , fieldSize :: ImVec2
  , buffSize  :: Int
  }

-- | Create an empty text field given its name, visual size, and maximum string length.
emptyTextField :: Text -> ImVec2 -> Int -> TextField
emptyTextField = TextField (Dynamic NoEvent "")

-- | Make a multiline text widget.
--   Note: Length is constant. @dear-imgui@ does not yet support resizing buffers.
mkTextMultiline :: MonadIO m => SF m TextField TextField
mkTextMultiline = proc textField -> do
  arrM $ inputTextFieldMultiline -< textField


-- | Helper function for @mkTextMultiline@
inputTextFieldMultiline :: MonadIO m => TextField -> m TextField
inputTextFieldMultiline (TextField textDyn label fieldSize buffSize) = do

  -- Create an IORef to store the text temporarily
  let oldText = current textDyn
  ioRef <- liftIO $ newIORef oldText --"the quick brown fox jumps over the lazy dog"

  inputTextMultiline label ioRef buffSize fieldSize

  -- Get the altered text
  newText <- liftIO $ readIORef ioRef

  pure $
    TextField
    {
      textDyn = Dynamic (diffText oldText newText) newText
    , label = label
    , fieldSize = fieldSize
    , buffSize = buffSize --Note: without library support for callback
    }

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
