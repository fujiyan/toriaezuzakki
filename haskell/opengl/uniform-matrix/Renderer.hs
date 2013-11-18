module Renderer (
   Descriptor, init, display, worldMatrix
) where

import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Prelude hiding ( init )
import System.IO
import qualified LoadShaders as LS
import qualified UniformLinear as UL
import Linear
import Data.Time

-- TODO: Just for debugging, remove me later.
checkError :: String -> IO ()
checkError functionName = get errors >>= mapM_ reportError
  where
    reportError e =
        hPutStrLn stderr (showError e ++ " detected in " ++ functionName)
    showError (Error category message) =
        "GL error " ++ show category ++ " (" ++ message ++ ")"

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices Program

init :: IO Descriptor
init = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    let vertices =
            -- vertex attribute format : x, y, z, r, g, b, a
            [
            -- Triangle 1
              (-0.90), (-0.90), 0.0, 1.0, 0.0, 0.0, 1.0
            ,    0.85, (-0.90), 0.0, 0.0, 1.0, 0.0, 1.0
            , (-0.90),    0.85, 0.0, 0.0, 0.0, 1.0, 1.0
            -- Triangle 2
            ,    0.90, (-0.85), 0.0, 0.0, 1.0, 1.0, 1.0
            ,    0.90,    0.90, 0.0, 1.0, 0.0, 1.0, 1.0
            , (-0.85),    0.90, 0.0, 1.0, 1.0, 0.0, 1.0
            ] :: [GLfloat]
        numPositionElements = 3
        numColorElements = 4
        offsetPosition = 0
        offsetColor = offsetPosition + numPositionElements
        numVertices = (length vertices) `div` (numPositionElements + numColorElements)
        sizeElement = sizeOf (head vertices)
        sizeVertex = fromIntegral (sizeElement * (numPositionElements + numColorElements))

    arrayBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just arrayBuffer
    withArray vertices $ \ptr -> do
        let size = fromIntegral ((length vertices) * sizeElement)
        bufferData ArrayBuffer $= (size, ptr, StaticDraw)

    program <- LS.loadShaders [
        LS.ShaderInfo VertexShader (LS.FileSource "triangles.vert"),
        LS.ShaderInfo FragmentShader (LS.FileSource "triangles.frag")]
    currentProgram $= Just program

    let vPosition = AttribLocation 0
        vColor = AttribLocation 1

    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numPositionElements) Float sizeVertex (bufferOffset (offsetPosition * sizeElement)))
    vertexAttribPointer vColor $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (bufferOffset (offsetColor * sizeElement)))

    vertexAttribArray vPosition $= Enabled
    vertexAttribArray vColor $= Enabled

    checkError "init"
    return $ Descriptor triangles 0 (fromIntegral numVertices) program

worldMatrix :: UTCTime -> M44 GLfloat
worldMatrix t =
    let ms = (realToFrac $ utctDayTime t) * 1000
        r = (2 * pi / 2000) * ms
    in  V4 (V4 1 0 0 0) (V4 0 (cos r) (sin r) 0) (V4 0 (-sin r) (cos r) 0) (V4 0 0 0 1)

display :: Descriptor -> M44 GLfloat -> IO ()
display (Descriptor triangles firstIndex numVertices program) worldMat = do
    clear [ ColorBuffer ]
    bindVertexArrayObject $= Just triangles

    worldLocation <- get $ uniformLocation program "world"
    UL.uniformMatrix4f worldLocation $= worldMat

    drawArrays Triangles firstIndex numVertices
    flush
    checkError "display"
