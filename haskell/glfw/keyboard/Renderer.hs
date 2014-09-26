-- | Provides functionality of rendering the application model.
module Renderer
    ( Descriptor
    , initialize
    , terminate
    , render
    ) where


import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import Linear
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


-- | The byte size of a memory area that is converted from a list.
arrayByteSize :: (Storable a)
    => [a] -- ^ a list
    -> Int
arrayByteSize ls = (sizeOf (head ls)) * (length ls)


-- | Represents a set of OpenGL objects for rendering information.
data Descriptor = Descriptor
    BufferObject
    VertexArrayObject
    BufferObject
    Program
    ArrayIndex
    NumArrayIndices


-- | Initializes a buffer object.
initializeBuffer :: (Storable a) => BufferTarget -> [a] -> IO BufferObject
initializeBuffer t array = do
    buffer <- genObjectName
    bindBuffer t $= Just buffer

    withArray array $ \ptr -> do
        bufferData t $= (fromIntegral $ arrayByteSize array, ptr, StaticDraw)

    bindBuffer ElementArrayBuffer $= Nothing
    return buffer


-- | Initializes OpenGL objects.
initialize :: IO Descriptor
initialize = do

    -- meshes
    let vertices =
            -- vertex attribute format : x, y, z, r, g, b, a
            [
              (-0.90), (-0.90), 0.0, 1.0, 0.0, 0.0, 1.0
            ,    0.90, (-0.90), 0.0, 0.0, 1.0, 0.0, 1.0
            ,    0.90,    0.90, 0.0, 1.0, 1.0, 1.0, 1.0
            , (-0.90),    0.90, 0.0, 0.0, 0.0, 1.0, 1.0
            ] :: [GLfloat]
        numPositionElements = 3
        numColorElements = 4
        offsetPosition = 0
        offsetColor = offsetPosition + numPositionElements
        sizeElement = sizeOf (head vertices)
        sizeVertex = fromIntegral (sizeElement * (numPositionElements + numColorElements))

    let indices =
            [
              0, 1, 2
            , 2, 3, 0
            ] :: [GLushort]


    vertexBuffer <- initializeBuffer ArrayBuffer vertices


    attributes <- genObjectName
    bindVertexArrayObject $= Just attributes
    bindBuffer ArrayBuffer $= Just vertexBuffer

    let vPosition = AttribLocation 0
        vColor = AttribLocation 1

    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numPositionElements) Float sizeVertex (bufferOffset (offsetPosition * sizeElement)))
    vertexAttribPointer vColor $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (bufferOffset (offsetColor * sizeElement)))

    vertexAttribArray vPosition $= Enabled
    vertexAttribArray vColor $= Enabled

    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing


    indexBuffer <- initializeBuffer ElementArrayBuffer indices


    program <- LS.loadShaders
        [ LS.ShaderInfo VertexShader (LS.FileSource "rectangle.vert")
        , LS.ShaderInfo FragmentShader (LS.FileSource "rectangle.frag")
        ]
    currentProgram $= Just program


    checkError "initialize"

    return $ Descriptor vertexBuffer attributes indexBuffer program 0 (fromIntegral $ length indices)


-- | Terminates OpenGL objects.
terminate :: Descriptor -> IO ()
terminate (Descriptor vertexBuffer attributes indexBuffer program _ _) = do
    currentProgram $= Nothing

    shaders <- get $ attachedShaders program
    mapM_ releaseShader shaders
    deleteObjectName program

    deleteObjectName indexBuffer
    deleteObjectName attributes
    deleteObjectName vertexBuffer

    checkError "terminate"

  where
    releaseShader shader = do
        detachShader program shader
        deleteObjectName shader


-- | A world matrix for the rectangle.
worldMatrix :: AM.RectangleData -- data of the rectangle
    -> M44 GLfloat -- ^ a world matrix
worldMatrix (AM.RectangleData x y) =
    V4  (V4 1 0 0 (fx / 50))
        (V4 0 1 0 (fy / 50))
        (V4 0 0 1 0)
        (V4 0 0 0 1)
  where
    fx = realToFrac x
    fy = realToFrac y


-- | Renders the application model with a descriptor.
render :: Descriptor -- ^ a descriptor
    -> AM.RectangleData -- ^ data of the rectangle
    -> IO ()
render (Descriptor _ attributes indexBuffer program rectangleOffset rectangleNumIndices) td = do
    worldLocation <- get $ uniformLocation program "world"
    UL.uniformMatrix4fv worldLocation $= [worldMatrix td]

    clear [ ColorBuffer ]

    bindVertexArrayObject $= Just attributes
    bindBuffer ElementArrayBuffer $= Just indexBuffer

    drawElements Triangles rectangleNumIndices UnsignedShort (bufferOffset rectangleOffset)

    bindBuffer ElementArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing

    flush

    checkError "render"
