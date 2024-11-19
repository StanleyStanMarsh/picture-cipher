module Main (main) where

import Lib (caesarCipher, caesarDecipher)
import Data.Text (pack)

main :: IO ()
main = do
    putStrLn "Caesar Cipher"
    let text = pack "Hello World"
    let encodedText = caesarCipher text 3
    print encodedText
    print $ caesarDecipher encodedText 3
