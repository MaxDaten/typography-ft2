{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TupleSections   #-}

-- | simple haskell interface for freetype2
--
-- for error codes: https://hackage.haskell.org/package/freetype2-0.1.1/src/include/freetype/fterrdef.h
module Graphics.Font
    ( module Graphics.Font
    , module FontFace
    , module FontLibrary
    , module Graphics.Font.FontGlyph
    ) where

import Data.Map.Strict                              hiding ( map )
import Data.Traversable                             ( sequenceA )
import Control.Applicative

import Graphics.Font.FontLibrary    as FontLibrary  ( FontLibrary, withNewLibrary )
import Graphics.Font.FontFace       as FontFace
import Graphics.Font.FontGlyph
import Graphics.Font.BitmapLoader

import Codec.Picture

pt :: Num a => a -> a
pt = (*) 64

data FontDescriptor = FontDescriptor
    { charSize  :: CharSize             -- | pt (1/64 pixel)
    , deviceRes :: DeviceResolution     -- | for dpi calculation
    } deriving ( Show, Eq, Ord )


data Font = Font
    { fontName  :: !String
    , charMap   :: !(Map Char FontGlyph)
    , fontDescr :: !FontDescriptor
    , fontFace  :: !FontFace
    , fontLib   :: !(FontLibrary)
    }

data FontLoadMode = 
      Gray8
    | Monochrome


loadFont :: FontLibrary -> FilePath -> FontDescriptor -> IO Font
loadFont flib fontfile descr@FontDescriptor{..} = do

    face <- newFontFace flib fontfile 0 >>= setSizes charSize deviceRes

    indices <- getAllFaceCharIndices face
    cMap <- fromList <$> mapM (toGlyph face) indices

    let fontName = familyName face ++ "-" ++ styleName face
    return $ Font fontName cMap descr face flib

    where

    setSizes = flip . flip setFaceCharSize
    toGlyph face (gindex, char) = (char,) <$> loadGlyph face gindex [LoadDefault]


loadCharGlyph :: Font -> [LoadMode] -> Char -> IO FontGlyph
loadCharGlyph Font{fontFace} mode c = 
    getFaceGlyphIndex fontFace c >>= flip (loadGlyph fontFace) mode


generateCharImg :: Font -> FontLoadMode -> Char -> IO (Image Pixel8)
generateCharImg font mode char =
    case mode of
        Gray8      -> load grayLoader [LoadRender]
        Monochrome -> load monoLoader [LoadRender, LoadMonochrome]
    where
        load loader flags = loadFaceCharImage (fontFace font) char flags loader


generateAllCharImgs :: Font -> FontLoadMode -> IO (Map Char (Image Pixel8))
generateAllCharImgs font mode = sequenceA $ mapWithKey (\c _ -> charImg c) (charMap font) where 
    charImg = generateCharImg font mode

