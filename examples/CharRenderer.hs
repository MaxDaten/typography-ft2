{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (lookup)

import ReadArgs

import Data.Char

import Graphics.Font
import Codec.Picture

main :: IO ()
main = do
    ( fontFile :: FilePath, theChar  :: Char, fontSize :: Int ) <- readArgs
    
    withNewLibrary $ \lib -> do
        let descr = FontDescriptor
                { charSize = (0, pt fontSize)
                , deviceRes = (0, 100)
                }
        font <- loadFont lib fontFile descr
        img  <- generateCharImg font Monochrome theChar

        print $ "char: " ++ [theChar]
        print $ "size: " ++ show ( imageWidth img, imageHeight img )
        let outFile = "char-" ++ show (ord theChar) ++ ".png"
        
        print $ "out: " ++ show outFile
        writePng outFile img
