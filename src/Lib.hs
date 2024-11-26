module Lib
    ( caesarCipher
    , caesarDecipher
    , encodeTextToImage
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (ord, chr)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as TE
import Data.Bits (testBit, setBit, clearBit)
import Data.Word (Word8)

textToByteString :: T.Text -> B.ByteString
textToByteString = TE.encodeUtf8

caesarCipher :: T.Text -> Int -> T.Text
caesarCipher str key = T.map (\c -> chr . (+ key) . ord $ c) str

caesarDecipher :: T.Text -> Int -> T.Text
caesarDecipher str key = T.map (\c -> chr . (subtract key) . ord $ c) str

-- переводит байт в список битов
byteToBits :: Word8 -> [Int]
byteToBits byte = [if testBit byte i then 1 else 0 | i <- [7,6..0]]

-- переводит строку байтов в список со списками битов
byteStringToBits :: B.ByteString -> [[Int]]
byteStringToBits bs = map (byteToBits . fromIntegral) (B.unpack bs)

-- Заменяет i-й бит в байте на заданный бит (0 или 1)
replaceIBit :: Word8 -> Int -> Int -> Word8
replaceIBit byte n bit
    | bit == 1  = setBit byte n
    | bit == 0  = clearBit byte n
    | otherwise = error "Bit must be 0 or 1"

processFile :: FilePath -> Int -> IO ()
processFile filePath key = do
    content <- TIO.readFile filePath
    let ciphered = caesarCipher content key
    -- let byteString = textToByteString ciphered
    -- let bits = byteStringToBits byteString
    print content
    print "--------"
    print ciphered

encodeTextToImage :: FilePath -> IO ()
encodeTextToImage fileName = do
    imageFile <- B.readFile fileName
    glitched <- return imageFile
    let glitchedFileName = mconcat ["glitched_", fileName]
    B.writeFile glitchedFileName glitched
    putStrLn "converted"
