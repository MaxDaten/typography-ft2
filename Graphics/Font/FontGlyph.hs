module Graphics.Font.FontGlyph where

import Foreign hiding (newForeignPtr)
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Concurrent
import System.Mem.Weak

import Control.Monad
import Control.Applicative

import Data.Char

import Graphics.Rendering.FreeType.Internal as FT
import Graphics.Rendering.FreeType.Internal.Face as F
import Graphics.Rendering.FreeType.Internal.Glyph as GL
import Graphics.Rendering.FreeType.Internal.GlyphSlot as GS
import Graphics.Rendering.FreeType.Internal.Library as L
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT
import Graphics.Rendering.FreeType.Internal.GlyphMetrics as GM

import Graphics.Font.FontFace

type GlypMetrics = FT_Glyph_Metrics
data FontGlyph = FontGlyph
    { glyphIndex    :: GlyphIndex
    , glyphMetrics  :: GlypMetrics
    } -- leeek! the glyph isnt freed



loadGlyph :: FontFace -> GlyphIndex -> [LoadMode] -> IO FontGlyph
loadGlyph face gindex mode = do
    g <- newFontGlyphPtr
    withForeignPtr (faceFrgnPtr face) $ \fptr -> do
        errL <- ft_Load_Glyph fptr (fromIntegral gindex) (loadModeBits mode)
        when (errL /= 0) $ error $ "ft_Load_Glyph error: " ++ show errL
        
        -- recieve the glyph and its metrics from slot and store it in the ptr
        slot <- peek $ glyph fptr
        m <- peek $ GS.metrics slot
        errG <- ft_Get_Glyph slot g

        when (errG /= 0) $ error $ "ft_Get_Glyph error: " ++ show errG
        free g
        return $ FontGlyph gindex m
    where       
        newFontGlyphPtr :: IO (Ptr FT_Glyph)
        newFontGlyphPtr = malloc



getFaceGlyphIndex :: FontFace -> Char -> IO GlyphIndex
getFaceGlyphIndex face char =
    withForeignPtr (faceFrgnPtr face) $ \ptr -> 
        fromIntegral <$> (ft_Get_Char_Index ptr (fromIntegral $ ord char))

