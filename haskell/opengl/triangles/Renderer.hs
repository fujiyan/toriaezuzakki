module Renderer (
   Descriptor, init, display
) where


import Control.Monad
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Prelude hiding ( init )
import System.IO
import qualified LoadShaders as LS


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

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

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
    return $ Descriptor triangles 0 (fromIntegral numVertices)

display :: Descriptor -> IO ()
display (Descriptor triangles firstIndex numVertices) = do
    clear [ ColorBuffer ]
    bindVertexArrayObject $= Just triangles
    drawArrays Triangles firstIndex numVertices
    flush
    checkError "display"
