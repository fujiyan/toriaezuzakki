-- The main module of the application. Performs GLFW-specific initialization and others.
module Main ( main ) where


import Control.Concurrent
import Control.Monad
import qualified Graphics.UI.GLFW as GLFW
import System.Environment
import System.IO

import qualified Renderer as R


-- | The error handler to be called when a GLFW error occurs.
errorHandler :: GLFW.ErrorCallback
errorHandler error description = do
    hPutStrLn stderr $ (show error) ++ ": " ++ description


-- | The rendering loop.
renderingLoop
    :: GLFW.Window -- ^ the window handle
    -> IO () -- ^ rendering action
    -> IO ()
renderingLoop window render = do
    loop

  where
    loop = (GLFW.windowShouldClose window) >>= (flip unless) go

    go = do
        render
        GLFW.swapBuffers window
        GLFW.pollEvents
        threadDelay 100000 -- Suspends to reduce the CPU usage.
        loop


-- | The process after the createWindow.
afterCreateWindow
    :: GLFW.Window -- ^ the window handle
    -> IO ()
afterCreateWindow window = do
    GLFW.makeContextCurrent $ Just window
    GLFW.swapInterval 1

    desc <- R.initialize

    renderingLoop window (R.render desc)

    R.terminate desc

    GLFW.destroyWindow window


-- | The entry point of the application.
main :: IO ()
main = do
    progName <- getProgName

    GLFW.setErrorCallback $ Just errorHandler

    GLFW.init

    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
    GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
    GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core

    GLFW.createWindow 500 500 progName Nothing Nothing >>= maybe (return ()) afterCreateWindow

    GLFW.terminate
