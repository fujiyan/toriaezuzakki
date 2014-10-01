-- | Provides texImage functions with JuicyPixels.
module TexJuicyPixels (
    textureImage2D
) where


import Codec.Picture
import Codec.Picture.Types
import Data.Vector.Storable
import Graphics.Rendering.OpenGL hiding (pixelMap)


-- | Specifies a two-dimensional texture image.
textureImage2D :: TwoDimensionalTextureTarget t => t -> Level -> Border -> DynamicImage -> IO ()
textureImage2D target level border image =
    case image of
        ImageRGB8 (Image w h d) -> textureImage2DInternal RGB8 RGB UnsignedByte w h d
        ImageRGB16 (Image w h d) -> textureImage2DInternal RGB16 RGB UnsignedShort w h d
        ImageRGBA8 (Image w h d) -> textureImage2DInternal RGBA8 RGBA UnsignedByte w h d
        ImageRGBA16 (Image w h d) -> textureImage2DInternal RGBA16 RGBA UnsignedShort w h d
        ImageRGBF (Image w h d) -> textureImage2DInternal RGB32F RGB Float w h d

        -- Converts to RGB.
        ImageYCbCr8 img -> textureImage2D target level border (ImageRGB8 $ pixelMap convertPixel img)
        ImageCMYK8 img -> textureImage2D target level border (ImageRGB8 $ pixelMap convertPixel img)
        ImageCMYK16 img -> textureImage2D target level border (ImageRGB16 $ pixelMap convertPixel img)
        ImageY8 img -> textureImage2D target level border (ImageRGB8 $ pixelMap promotePixel img)
        ImageY16 img -> textureImage2D target level border (ImageRGB16 $ pixelMap promotePixel img)
        ImageYF img -> textureImage2D target level border (ImageRGBF $ pixelMap promotePixel img)
        ImageYA8 img -> textureImage2D target level border (ImageRGBA8 $ pixelMap promotePixel img)
        ImageYA16 img -> textureImage2D target level border (ImageRGBA16 $ pixelMap promotePixel img)

  where
    textureImage2DInternal pif pf dt w h d =
        unsafeWith d ((texImage2D target NoProxy level pif (TextureSize2D (fromIntegral w) (fromIntegral h)) border) . (PixelData pf dt))
