{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Graphics.Font where

import Foreign hiding (newForeignPtr, unsafePerformIO)
import System.IO.Unsafe
import System.Mem.Weak
import Data.Map.Strict hiding (map)
import Data.Tuple
import Data.Char
import Control.Applicative

import Graphics.Font.FontLibrary
import Graphics.Font.FontFace
import Graphics.Font.FontGlyph
import Graphics.Font.BitmapLoader

import Codec.Picture

data FontDescriptor = FontDescriptor
    { charSize  :: CharSize             -- | pt (1/64 pixel)
    , deviceRes :: DeviceResolution     -- | for dpi calculation
    }


data Font = Font
    { fontname  :: String
    , charMap   :: Map Char GlyphIndex 
    , fontDescr :: FontDescriptor
    , fontFace  :: FontFace
    , libRef    :: Weak FontLibrary
    }

data FontLoadMode = 
      Gray8
    | Monochrome


loadFont :: FilePath -> FontDescriptor -> IO Font
loadFont fontfile descr@FontDescriptor{..} = do
    lib  <- makeLibrary
    face <- setSizes charSize deviceRes =<< newFontFace lib fontfile 0
    cMap <- fromList . map swap  <$> getAllFaceCharIndices face
    ref  <- mkWeak face lib (Just $ freeLibrary lib)
    let fontName = (familyName face) ++ "-" ++ (styleName face)
    return $ Font fontName cMap descr face ref
    where
        setSizes = flip . flip setFaceCharSize


loadCharGlyph :: Font -> [LoadMode] -> Char -> FontGlyph
loadCharGlyph Font{fontFace} mode c = 
    unsafePerformIO $ getFaceGlyphIndex fontFace c >>= flip (loadGlyph fontFace) mode


generateCharImg :: Font -> FontLoadMode -> Char -> Image Pixel8
generateCharImg font mode char = 
    case mode of
        Gray8      -> load grayLoader [LoadRender]
        Monochrome -> load monoLoader [LoadRender, LoadMonochrome]
    where
        load loader flags = unsafePerformIO $ loadFaceCharImage (fontFace font) char flags loader


generateAllCharImgs :: Font -> FontLoadMode -> Map Char (FontGlyph, Image Pixel8)
generateAllCharImgs font mode = mapWithKey (\c _ -> (fontGlyph c, charImg c)) (charMap font)
    where 
        charImg = generateCharImg font mode
        fontGlyph = loadCharGlyph font [LoadDefault] -- here is no rendering needed

