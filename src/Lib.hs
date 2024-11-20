module Lib
    ( caesarCipher
    , caesarDecipher
    , textToBS
    , convert
    ) where

import qualified Data.Text as T
import Data.Char (ord, chr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as TE


caesarCipher :: T.Text -> Int -> T.Text
caesarCipher str key = T.map (\c -> chr . (+ key) . ord $ c) str

caesarDecipher :: T.Text -> Int -> T.Text
caesarDecipher str key = T.map (\c -> chr . (subtract key) . ord $ c) str

convert :: FilePath -> IO ()
convert fileName = do
    imageFile <- BC.readFile fileName
    glitched <- return imageFile
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    putStrLn "converted"

textToBS :: T.Text -> B.ByteString
textToBS str = TE.encodeUtf8 str

intToChar :: Int -> Char
intToChar i = toEnum safeInt
    where
        safeInt = i `mod` 255

intToBC :: Int -> BC.ByteString
intToBC i = BC.pack [intToChar i]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc chV bytes = mconcat [before, newChar, after]
    where 
        (before, rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC chV


