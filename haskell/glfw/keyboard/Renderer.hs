-- | Provides functionality of rendering the application model.
module Renderer
    ( Descriptor
    , init
    , render
    ) where


import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Linear
import Prelude hiding ( init )
import System.IO

import qualified ApplicationModel as AM
import qualified LoadShaders as LS
import qualified UniformLinear as UL


-- | Checks OpenGL errors, and Writes to stderr when errors occur.
checkError
    :: String -- ^ a function name that called this
    -> IO ()
checkError functionName = get errors >>= mapM_ reportError
  where
    reportError (Error category message) = do
        hPutStrLn stderr $ (show category) ++ " in " ++ functionName ++ ": " ++ message


-- | Converts an offset value to the Ptr value.
bufferOffset :: Integral a
    => a -- ^ an offset value
    -> Ptr b -- ^ the Ptr value
bufferOffset = plusPtr nullPtr . fromIntegral


-- | Represents a set of OpenGL objects for rendering information.
data Descriptor = Descriptor
    VertexArrayObject
    ArrayIndex
    NumArrayIndices
    Program


-- | Initializes a descriptor.
init :: IO Descriptor
init = do
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- meshes
    let vertices =
            -- vertex attribute format : x, y, z, r, g, b, a
            [
            -- a triangle 1
              (-0.90), (-0.90), 0.0, 1.0, 0.0, 0.0, 1.0
            ,    0.85, (-0.90), 0.0, 0.0, 1.0, 0.0, 1.0
            , (-0.90),    0.85, 0.0, 0.0, 0.0, 1.0, 1.0
            -- a triangle 2
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

    program <- LS.loadShaders
        [ LS.ShaderInfo VertexShader (LS.FileSource "triangles.vert")
        , LS.ShaderInfo FragmentShader (LS.FileSource "triangles.frag")
        ]
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


-- | A world matrix for the triangles.
worldMatrix :: AM.TriangleData -- data of the triangles
    -> M44 GLfloat -- ^ a world matrix
worldMatrix (AM.TriangleData x y) =
    V4  (V4 1 0 0 (fx / 50))
        (V4 0 1 0 (fy / 50))
        (V4 0 0 1 0)
        (V4 0 0 0 1)
  where
    fx = realToFrac x
    fy = realToFrac y


-- | Renders the application model with a descriptor.
render :: Descriptor -- ^ a descriptor
    -> AM.TriangleData -- ^ data of the triangles
    -> IO ()
render (Descriptor triangles firstIndex numVertices program) td = do
    worldLocation <- get $ uniformLocation program "world"
    UL.uniformMatrix4fv worldLocation $= [worldMatrix td]

    clear [ ColorBuffer ]

    bindVertexArrayObject $= Just triangles
    drawArrays Triangles firstIndex numVertices

    flush

    checkError "display"
