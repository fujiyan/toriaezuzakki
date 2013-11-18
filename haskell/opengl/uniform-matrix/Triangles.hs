import Graphics.UI.GLUT
import qualified Renderer as R
import Data.Time


timer :: R.Descriptor -> TimerCallback
timer descriptor = do
    t <- getCurrentTime
    displayCallback $= R.display descriptor (R.worldMatrix t)

    postRedisplay Nothing

    addTimerCallback 20 (timer descriptor)

main :: IO ()
main = do
    (progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [ RGBAMode ]
    initialWindowSize $= Size 512 512
    initialContextVersion $= (3, 3)

    -- TODO: Just for debugging, remove me later.
    initialContextFlags $= [ DebugContext ]

    initialContextProfile $= [ CoreProfile ]
    createWindow progName
    descriptor <- R.init
    addTimerCallback 0 (timer descriptor)
    mainLoop
