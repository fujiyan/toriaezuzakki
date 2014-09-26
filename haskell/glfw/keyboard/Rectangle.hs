-- The main module of the application. Performs GLFW-specific initialization and others.
module Main ( main ) where


import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Graphics.UI.GLFW as GLFW
import System.Environment
import System.IO

import qualified ApplicationModel as AM
import qualified FixedStepManager as FSM
import qualified Renderer as R


-- | The error handler to be called when a GLFW error occurs.
errorHandler :: GLFW.ErrorCallback
errorHandler error description = do
    hPutStrLn stderr $ (show error) ++ ": " ++ description


-- | Scans a key which is being pressed.
scanKeyPress
    :: GLFW.Window -- ^ the window handle
    -> GLFW.Key -- ^ a code of a key which is scanned
    -> IO Bool -- ^ True indicates the key is being pressed
scanKeyPress window key = (== GLFW.KeyState'Pressed) <$> (GLFW.getKey window key)


-- | Updates the application model.
update
    :: GLFW.Window -- ^ the window handle
    -> AM.RectangleData -- ^ data before updating
    -> IO AM.RectangleData -- ^ data after updating
update window (AM.RectangleData x y) = do
    l <- (toValue (-1)) <$> (scanKeyPress window GLFW.Key'A)
    r <- (toValue 1) <$> (scanKeyPress window GLFW.Key'D)
    u <- (toValue 1) <$> (scanKeyPress window GLFW.Key'W)
    d <- (toValue (-1)) <$> (scanKeyPress window GLFW.Key'X)

    return $ AM.RectangleData (x + l + r) (y + u + d)

  where
    toValue v True = v
    toValue _ False = 0


-- | The rendering loop.
renderingLoop
    :: GLFW.Window -- ^ the window handle
    -> (AM.RectangleData -> IO ()) -- ^ rendering action
    -> IO ()
renderingLoop window render = do
    GLFW.setTime 0
    FSM.runStepManager (1/60) (loop (AM.RectangleData 0 0))

  where
    loop rd = ((lift . GLFW.windowShouldClose) window) >>= (flip unless) (go rd)

    getTime = GLFW.getTime >>= maybe (throwIO $ userError "getTime") (\t -> return t)

    go rd = do
        t <- lift getTime
        fp <- FSM.checkNextAction t
        case fp of
            FSM.None     -> do
                (lift . threadDelay) 10 -- Suspends to reduce the CPU usage.
                loop rd
            FSM.Update -> do
                rd' <- FSM.doUpdate (update window rd)
                loop rd'
            FSM.Drawing  -> do
                FSM.doDrawing (render rd)
                (lift . GLFW.swapBuffers) window
                lift GLFW.pollEvents
                loop rd


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
