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

loadFont :: FilePath -> FontDescriptor -> IO Font
loadFont fontfile descr@FontDescriptor{..} = do
    lib  <- makeLibrary
    face <- setSizes charSize deviceRes =<< newFontFace lib fontfile 0
    cMap <- fromList . map swap  <$> getAllFaceCharIndices face
    ref  <- mkWeak face lib (Just $ freeLibrary lib)
    return $ Font cMap descr face ref
    where
        setSizes = flip . flip setFaceCharSize

generateCharImg :: Font -> Char -> Image Pixel8
generateCharImg font char = unsafePerformIO $
    loadFaceCharImage (fontFace font) char LoadRender

generateAllCharImgs :: Font -> Map Char (Image Pixel8)
generateAllCharImgs font = mapWithKey (\c _ -> generateCharImg font c) (charMap font)