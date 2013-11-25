module Graphics.Font.BitmapLoader where

import Foreign


import Graphics.Rendering.FreeType.Internal.Bitmap as B

import Codec.Picture


type FontBitmapLoader m pixel = FT_Bitmap -> Int -> Int -> m pixel



monoLoader :: FontBitmapLoader IO Pixel8
monoLoader bitmap x y =
    let p                 = fromIntegral $ pitch bitmap
        (rowByte, inByte) = x `divMod` 8
        index             = y * p + rowByte 
    in do
        byte <- peek $ buffer bitmap `plusPtr` fromIntegral index :: IO Word8
        return $ 
            if testBit byte (7 - inByte)  -- the pixels are stored in most significant order
            then maxBound
            else minBound


grayLoader :: FontBitmapLoader IO Pixel8
grayLoader bitmap x y =
    let w       = fromIntegral $ width bitmap
        index   = y * w + x 
    in peek $ buffer bitmap `plusPtr` fromIntegral index
