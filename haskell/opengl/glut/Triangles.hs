import Graphics.UI.GLUT
import qualified Renderer as R


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
    displayCallback $= R.display descriptor
    mainLoop
