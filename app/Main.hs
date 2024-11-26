module Main (main) where

import Lib (encodeTextToImage)
import Data.Text (pack)
import qualified Data.ByteString as BS

main :: IO ()
main = do
    encodeTextToImage "ciol.bmp"