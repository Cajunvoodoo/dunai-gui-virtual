-- module Main where

-- import FRP.GUI.Widgets as W


-- main :: IO ()
-- main = W.main
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import FRP.GUI.Widgets as W
import Control.Monad.Free (Free, retract)

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged do
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

    liftIO $ mainLoop' window


mainLoop' :: Window -> IO ()
mainLoop' window = reactimate' @IO (testWidget window)

mainLoop :: Window -> IO ()
mainLoop window = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL3NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  setNextWindowFullscreen
  withWindowOpen "Hello, ImGui!" do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    fstBtn <- button "Clickety Click" >>= \case
      False -> return ()
      True  -> do
        putStrLn "Ow!"
        openPopup "Child window"
        pure ()
    withPopupModalOpen "Child window" $ do
      text "Child window!!"
      button "Other clicky"
        >>= \case
        False -> return ()
        True  -> putStrLn "Clicked!" >> closeCurrentPopup

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  render
  openGL3RenderDrawData =<< getDrawData

  glSwapWindow window

  threadDelay 10000

  mainLoop window

  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent

tshow :: Show a => a -> Text
tshow = T.pack . show
