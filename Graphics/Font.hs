{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE DeriveGeneric   #-}

-- | simple haskell interface for freetype2
--
-- for error codes: https://hackage.haskell.org/package/freetype2-0.1.1/src/include/freetype/fterrdef.h
module Graphics.Font
    ( module Graphics.Font
    , module FontFace
    , module FontLibrary
    , module Graphics.Font.FontGlyph
    ) where

import GHC.Generics

import Data.Map.Strict                              hiding ( map )
import Data.Traversable                             ( sequenceA )
import Control.Applicative

import Graphics.Font.FontLibrary    as FontLibrary  ( FontLibrary
                                                    , withNewLibrary
                                                    , makeLibrary
                                                    , freeLibrary )
import Graphics.Font.FontFace       as FontFace
import Graphics.Font.FontGlyph
import Graphics.Font.BitmapLoader

import Codec.Picture

pt :: Num a => a -> a
pt = (*) 64

data FontDescriptor = FontDescriptor
    { charSize  :: CharSize             -- | pt (1/64 pixel)
    , deviceRes :: DeviceResolution     -- | for dpi calculation
    } deriving ( Show, Eq, Ord, Generic )


data Font = Font
    { fontName  :: !String
    , charMap   :: !(Map Char FontGlyph)
    , fontDescr :: !FontDescriptor
    , fontFace  :: !(FontFaceFPtr,FontFace)
    , fontLib   :: !(FontLibrary)
    }

data FontLoadMode = 
      Gray8
    | Monochrome
    deriving ( Show, Eq, Ord, Enum, Generic )


loadFont :: FontLibrary -> FilePath -> FontDescriptor -> IO Font
loadFont flib fontfile descr@FontDescriptor{..} = do

    (fpt,face) <- newFontFace flib fontfile 0
    setFaceCharSize fpt charSize deviceRes 

    indices <- getAllFaceCharIndices fpt
    cMap <- fromList <$> mapM (toGlyph fpt) indices

    let fontName = familyName face ++ "-" ++ styleName face
    return $ Font fontName cMap descr (fpt,face) flib

    where

    toGlyph face (gindex, char) = (char,) <$> loadGlyph face gindex [LoadDefault]


loadCharGlyph :: Font -> [LoadMode] -> Char -> IO FontGlyph
loadCharGlyph Font{fontFace} mode c = 
    getFaceGlyphIndex (fst fontFace) c >>= flip (loadGlyph (fst fontFace)) mode


generateCharImg :: Font -> FontLoadMode -> Char -> IO (Image Pixel8)
generateCharImg font mode char =
    case mode of
        Gray8      -> load grayLoader [LoadRender]
        Monochrome -> load monoLoader [LoadRender, LoadMonochrome]
    where
    load loader flags = loadFaceCharImage (fst $ fontFace font) char flags loader


generateAllCharImgs :: Font -> FontLoadMode -> IO (Map Char (Image Pixel8))
generateAllCharImgs font mode = sequenceA $ mapWithKey (\c _ -> charImg c) (charMap font) where 
    charImg = generateCharImg font mode

