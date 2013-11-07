{-# LANGUAGE RecordWildCards #-}
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
import Graphics.Font.BitmapLoader

import Codec.Picture

data FontDescriptor = FontDescriptor
    { charSize  :: CharSize             -- | pt (1/64 pixel)
    , deviceRes :: DeviceResolution     -- | for dpi calculation
    }


data Font = Font
    { charMap   :: Map Char GlyphIndex 
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
    return $ Font cMap descr face ref
    where
        setSizes = flip . flip setFaceCharSize

generateCharImg :: Font -> Char -> FontLoadMode -> Image Pixel8
generateCharImg font char mode = 
    case mode of
        Gray8      -> load grayLoader [LoadRender]
        Monochrome -> load monoLoader [LoadRender, LoadMonochrome]
    where
        load loader flags = unsafePerformIO $ loadFaceCharImage (fontFace font) char flags loader


generateAllCharImgs :: Font -> FontLoadMode -> Map Char (Image Pixel8)
generateAllCharImgs font mode = mapWithKey (\c _ -> generateCharImg font c mode) (charMap font)