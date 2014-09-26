-- The main module of the application. Performs GLFW-specific initialization and others.
module Main ( main ) where


import Control.Concurrent
import Control.Exception
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
    -> (Double -> IO ()) -- ^ rendering action
    -> IO ()
renderingLoop window render = do
    GLFW.setTime 0
    loop

  where
    loop = (GLFW.windowShouldClose window) >>= (flip unless) go

    getTime = GLFW.getTime >>= maybe (throwIO $ userError "getTime") (\t -> return t)

    go = do
        t <- getTime
        let angle = (2 * pi / 2) * t -- Makes one rotation every two seconds.

        render angle
        GLFW.swapBuffers window
        GLFW.pollEvents
        threadDelay 16000 -- Suspends to reduce the CPU usage.
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
