{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
module Graphics.Font.FontGlyph
    ( module Graphics.Font.FontGlyph
    , module GM
    ) where

import Foreign hiding (newForeignPtr)
import GHC.Generics

import Control.Monad
import Control.Applicative

import Data.Char

import Graphics.Rendering.FreeType.Internal as FT
import Graphics.Rendering.FreeType.Internal.Face as F hiding (height)
import Graphics.Rendering.FreeType.Internal.Glyph as GL
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM

import Graphics.Font.FontFace

data GlyphMetrics = GlyphMetrics
    { glyWidth         :: Integer
    , glyHeight        :: Integer
    , glyHoriBearingX  :: Integer
    , glyHoriBearingY  :: Integer
    , glyHoriAdvance   :: Integer
    , glyVertBearingX  :: Integer
    , glyVertBearingY  :: Integer
    , glyVertAdvance   :: Integer
    } deriving ( Show, Eq, Generic )

data FontGlyph = FontGlyph
    { glyphIndex    :: GlyphIndex
    , glyphMetrics  :: GlyphMetrics
    } deriving ( Show, Eq, Generic )



loadGlyph :: FontFaceFPtr -> GlyphIndex -> [LoadMode] -> IO FontGlyph
loadGlyph fontface gindex mode = do
    g <- newFontGlyphPtr
    withForeignPtr fontface $ \fptr -> do
        errL <- ft_Load_Glyph fptr (fromIntegral gindex) (loadModeBits mode)
        when (errL /= 0) $ error $ "ft_Load_Glyph error: " ++ show errL
        
        -- recieve the glyph and its metrics from slot and store it in the ptr
        slot <- peek $ glyph fptr
        ftMetric <- peek $ GS.metrics slot
        errG <- ft_Get_Glyph slot g

        when (errG /= 0) $ error $ "ft_Get_Glyph error: " ++ show errG
        free g
        return $ FontGlyph gindex $ toHSMetrics ftMetric
    where       
        newFontGlyphPtr :: IO (Ptr FT_Glyph)
        newFontGlyphPtr = malloc



getFaceGlyphIndex :: FontFaceFPtr -> Char -> IO GlyphIndex
getFaceGlyphIndex fontface char =
    withForeignPtr fontface $ \ptr -> 
        fromIntegral <$> ft_Get_Char_Index ptr (fromIntegral $ ord char)


toHSMetrics :: FT_Glyph_Metrics -> GlyphMetrics
toHSMetrics FT_Glyph_Metrics{..} = GlyphMetrics
    { glyWidth         = fromIntegral width
    , glyHeight        = fromIntegral height
    , glyHoriBearingX  = fromIntegral horiBearingX
    , glyHoriBearingY  = fromIntegral horiBearingY
    , glyHoriAdvance   = fromIntegral horiAdvance
    , glyVertBearingX  = fromIntegral vertBearingX
    , glyVertBearingY  = fromIntegral vertBearingY
    , glyVertAdvance   = fromIntegral vertAdvance
    }
