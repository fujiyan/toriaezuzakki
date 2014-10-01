-- | Provides functionality of rendering the application model.
module Renderer
    ( Descriptor
    , initialize
    , terminate
    , render
    ) where


import Codec.Picture
import Control.Exception
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL
import System.IO

import qualified LoadShaders as LS
import qualified SamplerObjects as SO
import qualified TexJuicyPixels as TJP


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
    TextureObject
    SO.SamplerObject
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
            -- vertex attribute format : x, y, z, r, g, b, a, s, t
            [
              (-0.90), (-0.90), 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 1.0
            ,    0.90, (-0.90), 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0
            ,    0.90,    0.90, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0
            , (-0.90),    0.90, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0
            ] :: [GLfloat]
        numPositionElements = 3
        numColorElements = 4
        numTexCoordElements = 2
        offsetPosition = 0
        offsetColor = offsetPosition + numPositionElements
        offsetTexCoord = offsetColor + numColorElements
        sizeElement = sizeOf (head vertices)
        sizeVertex = fromIntegral (sizeElement * (numPositionElements + numColorElements + numTexCoordElements))

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
        vTexCoord = AttribLocation 2

    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numPositionElements) Float sizeVertex (bufferOffset (offsetPosition * sizeElement)))
    vertexAttribPointer vColor $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numColorElements) Float sizeVertex (bufferOffset (offsetColor * sizeElement)))
    vertexAttribPointer vTexCoord $=
        (ToFloat, VertexArrayDescriptor (fromIntegral numTexCoordElements) Float sizeVertex (bufferOffset (offsetTexCoord * sizeElement)))

    vertexAttribArray vPosition $= Enabled
    vertexAttribArray vColor $= Enabled
    vertexAttribArray vTexCoord $= Enabled

    bindBuffer ArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing


    indexBuffer <- initializeBuffer ElementArrayBuffer indices


    texture <- genObjectName
    textureBinding Texture2D $= Just texture
    r <- readImage "texture.jpg"
    case r of
        Left s -> throwIO $ userError s
        Right img -> TJP.textureImage2D Texture2D 0 0 img
    textureBinding Texture2D $= Nothing

    sampler <- genObjectName
    SO.samplerFilter sampler $= ((Nearest, Nothing), Nearest)
    SO.samplerWrapMode sampler S $= (Repeated, Repeat)
    SO.samplerWrapMode sampler T $= (Repeated, Repeat)


    program <- LS.loadShaders
        [ LS.ShaderInfo VertexShader (LS.FileSource "rectangle.vert")
        , LS.ShaderInfo FragmentShader (LS.FileSource "rectangle.frag")
        ]
    currentProgram $= Just program


    checkError "initialize"

    return $ Descriptor vertexBuffer attributes indexBuffer program texture sampler 0 (fromIntegral $ length indices)


-- | Terminates OpenGL objects.
terminate :: Descriptor -> IO ()
terminate (Descriptor vertexBuffer attributes indexBuffer program texture sampler _ _) = do
    currentProgram $= Nothing

    shaders <- get $ attachedShaders program
    mapM_ releaseShader shaders
    deleteObjectName program

    deleteObjectName sampler
    deleteObjectName texture

    deleteObjectName indexBuffer
    deleteObjectName attributes
    deleteObjectName vertexBuffer

    checkError "terminate"

  where
    releaseShader shader = do
        detachShader program shader
        deleteObjectName shader


-- | Renders the application model with a descriptor.
render :: Descriptor -- ^ a descriptor
    -> IO ()
render (Descriptor _ attributes indexBuffer program texture sampler rectangleOffset rectangleNumIndices) = do
    clear [ ColorBuffer ]

    activeTexture $= (TextureUnit 0)
    textureBinding Texture2D $= Just texture
    SO.samplerBinding 0 $= Just sampler
    texture <- get $ uniformLocation program "texture"
    uniform texture $= (TextureUnit 0)

    bindVertexArrayObject $= Just attributes
    bindBuffer ElementArrayBuffer $= Just indexBuffer

    drawElements Triangles rectangleNumIndices UnsignedShort (bufferOffset rectangleOffset)

    bindBuffer ElementArrayBuffer $= Nothing
    bindVertexArrayObject $= Nothing

    SO.samplerBinding 0 $= Nothing
    textureBinding Texture2D $= Nothing

    flush

    checkError "render"
