module Lib
    ( caesarCipher
    , caesarDecipher
    ) where

import qualified Data.Text as T
import Data.Char (ord, chr)

caesarCipher :: T.Text -> Int -> T.Text
caesarCipher str key = T.map (\c -> chr . (+ key) . ord $ c) str

caesarDecipher :: T.Text -> Int -> T.Text
caesarDecipher str key = T.map (\c -> chr . (subtract key) . ord $ c) str
