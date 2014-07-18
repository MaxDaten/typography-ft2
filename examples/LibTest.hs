{-# LANGUAGE BangPatterns #-}
{--
http://www.freetype.org/freetype2/docs/tutorial/step1.html
--}
module Main where

import Prelude hiding (lookup)

import Control.Monad
import Control.Applicative

import System.FilePath

import Data.Char
import Data.Map (toList, lookup, size)

import Graphics.Font
import Codec.Picture

{--
" !\"#$%&'()*+,-./0123456789:;<=>?"
"@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
"`abcdefghijklmnopqrstuvwxyz{|}~"
--}

fontPath :: FilePath
fontPath = "font" </> "SourceCodePro-Regular.otf"
charFolder :: FilePath
charFolder = "chars"

main :: IO ()
main = withNewLibrary $ \lib -> do
    let descr = FontDescriptor
            { charSize = (0, 16*64)
            , deviceRes = (600, 600)
            }
    font <- loadFont lib fontPath descr
    imgs <- generateAllCharImgs font Monochrome

    print $ "_loaded chars: " ++ show (size imgs)
    forM_ (toList imgs) $ \(!c, !img) -> do
        print c
        print $ glyphMetrics <$> lookup c (charMap font)
        writePng (charFolder </> "char" ++ show (ord c) ++ ".png") img
