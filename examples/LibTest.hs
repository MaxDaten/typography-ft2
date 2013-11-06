{--
http://www.freetype.org/freetype2/docs/tutorial/step1.html
--}
module Main where

import Foreign hiding (newForeignPtr, unsafePerformIO)
import Foreign.Marshal
import Foreign.Concurrent
import Foreign.C.String
import System.IO.Unsafe

import Control.Monad
import Control.Applicative

import System.FilePath

import Data.Char
import Data.Map (toList)

import Graphics.Font
import Codec.Picture



fontPath = "font" </> "SourceCodePro-Regular.otf"
charFolder = "chars"
main = do
    let descr = FontDescriptor
            { charSize = (0, 16*64)
            , deviceRes = (600, 600)
            }
    font <- loadFont fontPath descr
    let imgs = generateAllCharImgs font

    forM_ (toList imgs) $ \(c, img) -> do
        writePng (charFolder </> "char" ++ (show $ ord c) ++ ".png") img
