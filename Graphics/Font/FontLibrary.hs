module Graphics.Font.FontLibrary where


import Foreign hiding (newForeignPtr)
import Foreign.Marshal.MissingAlloc
import Control.Monad
import Control.Exception.Base


import Graphics.Rendering.FreeType.Internal as FT
import Graphics.Rendering.FreeType.Internal.Library as L


type FontLibrary = FT_Library


makeLibrary :: IO FontLibrary
makeLibrary = do
    libPtr <- calloc
    err    <- ft_Init_FreeType libPtr
    when (err /= 0) $ error $ "ft_Init_FreeType error: " ++ show err
    peek libPtr


freeLibrary :: FontLibrary -> IO ()
freeLibrary lib = do
    err <- ft_Done_FreeType lib
    when (err /= 0) $ error $ "ft_Done_FreeType error: " ++ show err


withNewLibrary :: (FontLibrary -> IO a) -> IO a
withNewLibrary = bracket makeLibrary freeLibrary