-- The main module of the application. Performs GLFW-specific initialization and others.
module Main ( main ) where


import Control.Concurrent
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import System.IO

import qualified Renderer as R


-- | The error handler to be called when a GLFW error occurs.
errorHandler :: GLFW.ErrorCallback
errorHandler error description = do
    hPutStrLn stderr $ (show error) ++ ": " ++ description


-- | The rendering loop.
renderingLoop
    :: GLFW.Window -- ^ a window handle
    -> IO () -- ^ a rendering action
    -> IO ()
renderingLoop window display = do
    loop

  where
    loop = (GLFW.windowShouldClose window) >>= (flip unless) go

    go = do
        display
        GLFW.swapBuffers window
        GLFW.pollEvents
        threadDelay 100000 -- suspends to reduce cpu usage
        loop


-- | The process after the createWindow.
afterCreateWindow
    :: GLFW.Window -- ^ a window handle
    -> IO ()
afterCreateWindow window = do
    GLFW.makeContextCurrent $ Just window
    GLFW.swapInterval 1

    desc <- R.init

    renderingLoop window (R.display desc)

    GLFW.destroyWindow window


-- | The entry point of the application.
main :: IO ()
main = do
    GLFW.setErrorCallback $ Just errorHandler

    GLFW.init

    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

    GLFW.createWindow 500 500 "Triangles" Nothing Nothing >>= maybe (return ()) afterCreateWindow

    GLFW.terminate
