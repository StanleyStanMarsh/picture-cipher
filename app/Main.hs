module Main (main) where

import Lib (caesarCipher, caesarDecipher, textToBS, convert)
import Data.Text (pack)
import qualified Data.ByteString as BS

main :: IO ()
main = do
    putStrLn "Caesar Cipher"
    let text = pack "Hello World"
    let encodedText = caesarCipher text 3
    print encodedText
    print $ caesarDecipher encodedText 3
    putStrLn "Bytes:"
    print $ BS.unpack $ textToBS text

    convert "sample.bmp"