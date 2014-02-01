-- The main module of the application.
module Main ( main ) where


import qualified Graphics.UI.GLFW as GLFW
import System.Environment
import System.IO


-- | The error handler to be called when a GLFW error occurs.
errorHandler :: GLFW.ErrorCallback
errorHandler error description = do
    hPutStrLn stderr $ (show error) ++ ": " ++ description


-- | The process after the createWindow.
afterCreateWindow
    :: GLFW.Window -- ^ a window handle
    -> IO ()
afterCreateWindow window = do
    GLFW.makeContextCurrent $ Just window

    major <- GLFW.getWindowContextVersionMajor window
    minor <- GLFW.getWindowContextVersionMinor window
    revision <- GLFW.getWindowContextVersionRevision window

    putStrLn $ "Version: " ++ (show major) ++ '.' : (show minor) ++ '.' : (show revision)

    GLFW.destroyWindow window


-- | The entry point of the application.
main :: IO ()
main = do
    progName <- getProgName

    GLFW.setErrorCallback $ Just errorHandler

    GLFW.init

    GLFW.createWindow 500 500 progName Nothing Nothing >>= maybe (return ()) afterCreateWindow

    GLFW.terminate
