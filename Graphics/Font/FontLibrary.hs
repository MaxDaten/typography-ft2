module Graphics.Font.FontLibrary where


import Foreign hiding (newForeignPtr, unsafePerformIO)

import Control.Monad


import Graphics.Rendering.FreeType.Internal as FT
import Graphics.Rendering.FreeType.Internal.Library as L


type FontLibrary = FT_Library


makeLibrary :: IO FontLibrary
makeLibrary = do
    libPtr <- malloc
    err    <- ft_Init_FreeType libPtr
    when (err /= 0) $ error $ "ft_Init_FreeType error: " ++ show err
    lib <- peek libPtr
    free libPtr
    return lib


freeLibrary :: FontLibrary -> IO ()
freeLibrary lib = do
    err <- ft_Done_FreeType lib
    when (err /= 0) $ error $ "ft_Done_FreeType error: " ++ show err
