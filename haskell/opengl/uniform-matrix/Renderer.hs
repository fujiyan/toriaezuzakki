-- | Provides functions and types for rendering.
module Renderer
    ( Descriptor
    , init
    , display
    ) where


import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Linear
import Prelude hiding ( init )
import System.IO

import qualified LoadShaders as LS
import qualified UniformLinear as UL


-- | Checks OpenGL errors, and Write to stderr when the errors occur.
checkError
    :: String -- ^ the function name that called this
    -> IO ()
checkError functionName = get errors >>= mapM_ reportError
  where
    reportError (Error category message) = do
        hPutStrLn stderr $ (show category) ++ " in " ++ functionName ++ ": " ++ message


-- | Converts a offset value to a Ptr value
bufferOffset :: Integral a
    => a -- ^ a offset value
    -> Ptr b -- ^ the Ptr value
bufferOffset = plusPtr nullPtr . fromIntegral


-- | A set of OpenGL objects which is set for the application and other rendering information.
data Descriptor = Descriptor
    VertexArrayObject
    ArrayIndex
    NumArrayIndices
    Program


-- | Initializes a 'Descriptor'.
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
worldMatrix :: GLfloat -- an angle of the triangles in radians
    -> M44 GLfloat -- a world matrix
worldMatrix angle =
    V4 (V4 1 0 0 0) (V4 0 (cos angle) (sin angle) 0) (V4 0 (-sin angle) (cos angle) 0) (V4 0 0 0 1)


-- | Displays meshes with a 'Descriptor'.
display :: Descriptor -- a 'Descriptor'
    -> Double -- an angle of the triangles in radians
    -> IO ()
display (Descriptor triangles firstIndex numVertices program) angle = do
    worldLocation <- get $ uniformLocation program "world"
    UL.uniformMatrix4fv worldLocation $= [worldMatrix $ realToFrac angle]

    clear [ ColorBuffer ]

    bindVertexArrayObject $= Just triangles
    drawArrays Triangles firstIndex numVertices

    flush

    checkError "display"
