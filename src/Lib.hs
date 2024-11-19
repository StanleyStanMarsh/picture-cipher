module Lib
    ( caesarCipher
    ) where

import qualified Data.Text as T
import Data.Char (ord, chr)

caesarCipher :: T.Text -> Int -> T.Text
caesarCipher str key = T.map (\c -> chr . (+ key) . ord $ c) str
