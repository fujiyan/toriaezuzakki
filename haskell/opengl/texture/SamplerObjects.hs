module SamplerObjects (
    SamplerObject,
    samplerBinding,
    samplerFilter,
    samplerWrapMode
) where


import Data.Maybe (fromMaybe)
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.Raw


newtype SamplerObject = SamplerObject { samplerID :: GLuint }
    deriving ( Eq, Ord, Show )


instance ObjectName SamplerObject where
    isObjectName = fmap (/= fromIntegral gl_FALSE) . glIsSampler . samplerID

    deleteObjectNames samplerObjects =
        withArrayLen (map samplerID samplerObjects) $
            glDeleteSamplers . fromIntegral


instance GeneratableObjectName SamplerObject where
    genObjectNames n =
        allocaArray n $ \buf -> do
            glGenSamplers (fromIntegral n) buf
            fmap (map SamplerObject) $ peekArray n buf


samplerBinding :: GLuint -> SettableStateVar (Maybe SamplerObject)
samplerBinding tu =
    makeSettableStateVar
        (glBindSampler tu . samplerID . (fromMaybe defaultSamplerObject))

defaultSamplerObject :: SamplerObject
defaultSamplerObject = SamplerObject 0


samplerFilter :: SamplerObject -> SettableStateVar (MinificationFilter, MagnificationFilter)
samplerFilter so =
    makeSettableStateVar (
        \(x, y) -> do
            glSamplerParameteri (samplerID so) gl_TEXTURE_MIN_FILTER (marshalFilter x)
            glSamplerParameteri (samplerID so) gl_TEXTURE_MAG_FILTER (marshalFilter (y, Nothing))
    )
  where
    marshalFilter x = fromIntegral $ case x of
        (Nearest, Nothing) -> gl_NEAREST
        (Linear', Nothing) -> gl_LINEAR
        (Nearest, Just Nearest) -> gl_NEAREST_MIPMAP_NEAREST
        (Linear', Just Nearest) -> gl_LINEAR_MIPMAP_NEAREST
        (Nearest, Just Linear') -> gl_NEAREST_MIPMAP_LINEAR
        (Linear', Just Linear') -> gl_LINEAR_MIPMAP_LINEAR


samplerWrapMode :: SamplerObject -> TextureCoordName -> SettableStateVar (Repetition, Clamping)
samplerWrapMode so coord = case coord of
    S -> wrap gl_TEXTURE_WRAP_S
    T -> wrap gl_TEXTURE_WRAP_T
    R -> wrap gl_TEXTURE_WRAP_R
    Q -> undefined
  where
    wrap c = makeSettableStateVar (glSamplerParameteri (samplerID so) c . marshalTextureWrapMode)

    marshalTextureWrapMode x = fromIntegral $ case x of
        (Repeated, Clamp) -> gl_CLAMP
        (Repeated, Repeat) -> gl_REPEAT
        (Repeated, ClampToEdge) -> gl_CLAMP_TO_EDGE
        (Repeated, ClampToBorder) -> gl_CLAMP_TO_BORDER
        (Mirrored, Clamp) -> gl_MIRROR_CLAMP
        (Mirrored, Repeat) -> gl_MIRRORED_REPEAT
        (Mirrored, ClampToEdge) -> gl_MIRROR_CLAMP_TO_EDGE
        (Mirrored, ClampToBorder) -> gl_MIRROR_CLAMP_TO_BORDER
