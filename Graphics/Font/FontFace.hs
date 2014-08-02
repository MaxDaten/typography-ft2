module Graphics.Font.FontFace where



import           Foreign                                             hiding (newForeignPtr, free)
import           Foreign.C.String
import           Foreign.Concurrent
import           System.IO

import           Control.Applicative
import           Control.Monad
import           Data.Char


import           Graphics.Rendering.FreeType.Internal                as FT
import           Graphics.Rendering.FreeType.Internal.Bitmap         as B
import           Graphics.Rendering.FreeType.Internal.Face           as F hiding (ascender, descender)
import qualified Graphics.Rendering.FreeType.Internal.Face           as F (ascender, descender)
import           Graphics.Rendering.FreeType.Internal.GlyphSlot      as GS
import           Graphics.Rendering.FreeType.Internal.PrimitiveTypes as PT

import           Graphics.Font.BitmapLoader
import           Graphics.Font.FontLibrary

import           Codec.Picture


type FontFaceFPtr = ForeignPtr FT_FaceRec_
data FontFace = FontFace
    { faceFrgnPtr   :: FontFaceFPtr
    , numFaces      :: Integer
    , faceIndex     :: Integer
    , faceFlags     :: Integer
    , styleFlags    :: Integer
    , numGlyphs     :: Integer
    , familyName    :: String
    , styleName     :: String
    , numFixedSizes :: Int
    -- availableSizes
    , numCharMaps   :: Int
    , ascender      :: Int -- Int16
    , descender     :: Int -- Int16
    , lineHeight    :: Int -- Int16
    , unitsPerEM    :: Int
    -- charmaps
    -- generic
    -- bbox
    -- ...
    } deriving (Show)


type FaceIndex = Int
type CharSize = (Int, Int)
type DeviceResolution = (Int, Int)
type GlyphIndex = Int


data LoadMode =
      LoadDefault
    | LoadNoScale
    | LoadNoHinting
    | LoadRender
    | LoadNoBitmap
    | LoadVerticalLayout
    | LoadForceAutohint
    | LoadCropBitmap
    | LoadPedantic
    | LoadIgnoreGlobalAdvanceWidth
    | LoadNoRecurse
    | LoadIgnoreTransform
    | LoadMonochrome
    | LoadLinearDesign
    | LoadNoAutohint
toLoadFlag :: LoadMode -> Int32
toLoadFlag LoadDefault                  = ft_LOAD_DEFAULT
toLoadFlag LoadNoScale                  = ft_LOAD_NO_SCALE
toLoadFlag LoadNoHinting                = ft_LOAD_NO_HINTING
toLoadFlag LoadRender                   = ft_LOAD_RENDER
toLoadFlag LoadNoBitmap                 = ft_LOAD_NO_BITMAP
toLoadFlag LoadVerticalLayout           = ft_LOAD_VERTICAL_LAYOUT
toLoadFlag LoadForceAutohint            = ft_LOAD_FORCE_AUTOHINT
toLoadFlag LoadCropBitmap               = ft_LOAD_CROP_BITMAP
toLoadFlag LoadPedantic                 = ft_LOAD_PEDANTIC
toLoadFlag LoadIgnoreGlobalAdvanceWidth = ft_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH
toLoadFlag LoadNoRecurse                = ft_LOAD_NO_RECURSE
toLoadFlag LoadIgnoreTransform          = ft_LOAD_IGNORE_TRANSFORM
toLoadFlag LoadMonochrome               = ft_LOAD_MONOCHROME
toLoadFlag LoadLinearDesign             = ft_LOAD_LINEAR_DESIGN
toLoadFlag LoadNoAutohint               = ft_LOAD_NO_AUTOHINT


newFontFace :: FontLibrary -> FilePath -> FaceIndex -> IO FontFace
newFontFace lib fontfile index = withBinaryFile fontfile ReadMode $ \hndl -> hndl `seq` do -- just dummy open for error checking
    withCString fontfile $ \file -> do
        facePtr <- malloc
        err     <- ft_New_Face lib file (fromIntegral index) facePtr
        when (err /= 0) $ error $ "ft_New_Face error: " ++ show err
        fPtr <- peek facePtr >>= \ptr -> newForeignPtr ptr (free ptr)
        FontFace fPtr
            <$> (fromIntegral <$> fPtr `doPeek` num_faces)
            <*> (fromIntegral <$> fPtr `doPeek` face_index)
            <*> (fromIntegral <$> fPtr `doPeek` face_flags)
            <*> (fromIntegral <$> fPtr `doPeek` style_flags)
            <*> (fromIntegral <$> fPtr `doPeek` num_glyphs)
            <*> (peekCString  =<< fPtr `doPeek` family_name)
            <*> (peekCString  =<< fPtr `doPeek` style_name)
            <*> (fromIntegral <$> fPtr `doPeek` num_fixed_sizes)
            <*> (fromIntegral <$> fPtr `doPeek` num_charmaps)
            <*> (fromIntegral <$> fPtr `doPeek` F.ascender)
            <*> (fromIntegral <$> fPtr `doPeek` F.descender)
            <*> (fromIntegral <$> fPtr `doPeek` height)
            <*> (fromIntegral <$> fPtr `doPeek` units_per_EM)
    where
    doPeek ptr f = withForeignPtr ptr (peek . f)
    free ptr = do
        err <- ft_Done_Face ptr
        when (err /= 0) $ error $ "ft_Done_Face error: " ++ show err


setFaceCharSize :: FontFace -> CharSize -> DeviceResolution -> IO FontFace
setFaceCharSize fontFace (charWidth, charHeigt) (devWidth, devHeight) =
    withForeignPtr (faceFrgnPtr fontFace) $ \ptr -> do
        err <- ft_Set_Char_Size ptr
                (fromIntegral charWidth) (fromIntegral charHeigt)
                (fromIntegral devWidth) (fromIntegral devHeight)
        when (err /= 0) $ error $ show ptr ++ "* ft_Set_CharSize error:: " ++ show err
        return fontFace


getAllFaceCharIndices :: FontFace -> IO [(GlyphIndex, Char)]
getAllFaceCharIndices fontFace =
    withForeignPtr (faceFrgnPtr fontFace) $ \facePtr ->
        alloca $ \gPtr -> do
            charCode    <- ft_Get_First_Char facePtr gPtr
            gidx        <- peek gPtr
            ls          <- getNext facePtr charCode gPtr
            return $ (fromIntegral gidx, chr . fromIntegral $ charCode):ls
    where
        getNext _ 0 _ = return []
        getNext facePtr c gPtr = do
            charC <- ft_Get_Next_Char facePtr c gPtr
            glypI <- peek gPtr
            ls    <- getNext facePtr charC gPtr
            return $ [(fromIntegral glypI, chr . fromIntegral $ charC) | glypI /= 0] ++ ls


loadFaceCharImage :: (Pixel a) => FontFace -> Char -> [LoadMode] -> FontBitmapLoader IO a -> IO (Image a)
loadFaceCharImage fontFace code mode imageLoader =
    withForeignPtr (faceFrgnPtr fontFace) $ \ptr -> do

        err     <- ft_Load_Char ptr (fromIntegral . ord $ code) (loadModeBits mode)
        when (err /= 0) $ error $ "ft_Set_CharSize error: " ++ show err

        bm      <- peek . bitmap =<< peek (glyph ptr)

        let w = fromIntegral $ width bm
            h = fromIntegral $ rows bm
        withImage w h (imageLoader bm)


loadModeBits :: [LoadMode] -> Int32
loadModeBits = foldr ((.|.) . toLoadFlag) 0

