module Main (main) where

import Lib (caesarCipher)
import Data.Text (pack)

main :: IO ()
main = do
    putStrLn "Caesar Cipher"
    let text = pack "Hello World"
    print $ caesarCipher text 3
