{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TupleSections   #-}
module Graphics.Font
    ( module Graphics.Font
    , module Graphics.Font.FontGlyph
    ) where

import Foreign hiding (newForeignPtr, unsafePerformIO)
import System.IO.Unsafe
import System.Mem.Weak
import Data.Map.Strict hiding (map)
import Data.Tuple (swap)
import Data.Char
import Control.Applicative
import Control.Monad

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
    , charMap   :: Map Char FontGlyph 
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
    face <- newFontFace lib fontfile 0 >>= setSizes charSize deviceRes

    indices <- getAllFaceCharIndices face
    cMap <- fromList <$> mapM (toGlyph face) indices
    
    ref  <- mkWeak face lib (Just $ freeLibrary lib)
    let fontName = (familyName face) ++ "-" ++ (styleName face)
    return $ Font fontName cMap descr face ref
    where
        setSizes = flip . flip setFaceCharSize
        toGlyph face (gindex, char) = (char,) <$> loadGlyph face gindex [LoadDefault]


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


generateAllCharImgs :: Font -> FontLoadMode -> Map Char (Image Pixel8)
generateAllCharImgs font mode = mapWithKey (\c _ -> charImg c) (charMap font)
    where 
        charImg = generateCharImg font mode

