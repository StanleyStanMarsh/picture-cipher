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
import Codec.BMP

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

-- Разбивает список на части длиной k
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)

replaceBitsInByteString :: B.ByteString -> B.ByteString -> Int -> B.ByteString
replaceBitsInByteString original replacing n
    | n < 1 || n > 8 = error "Number of bits to replace must be between 1 and 8"
    | B.length replacing * 8 > B.length original * n = error "Not enough bits in original"
    | otherwise = B.pack $ go (B.unpack original) (chunksOf n replacingBits)
  where
    -- Преобразуем байты из replacing в список битов
    replacingBits = concatMap byteToBits (B.unpack replacing)

    -- Рекурсивная функция для замены битов и сохранения неизмененных байтов
    go [] [] = []  -- Все байты обработаны
    go (o:os) [] = o : go os []  -- Добавляем оставшиеся байты из original, если нет замены
    go (o:os) (r:rs) = 
        let modifiedByte = replaceNthBits o r  -- Заменяем последние n бит
        in modifiedByte : go os rs  -- Добавляем измененный байт и рекурсивно обрабатываем остаток
    
    
    -- Заменяет последние n бит байта на новые биты из replacing
    replaceNthBits :: Word8 -> [Int] -> Word8
    replaceNthBits byte newBits =
        foldl (\acc (bit, idx) -> replaceIBit acc idx bit) byte (zip newBits [8-n..7])

encodeTextToImage :: FilePath -> IO ()
encodeTextToImage fileName = do
    Right bmp  <- readBMP fileName
    let rgba = unpackBMPToRGBA32 bmp
    let (width, height) = bmpDimensions bmp
    -- imageFile <- BC.readFile fileName
    textFile <- B.readFile "bio.txt"
    
    -- Функция для генерации имени файла
    let glitchedFileName n = mconcat [show n, "_", fileName]
    
    -- Итерация по значениям от 1 до 8
    mapM_ (\n -> do
        let modifiedRGBA = B.reverse $ replaceBitsInByteString (B.reverse rgba) textFile n
        let glitchedBMP = packRGBA32ToBMP width height modifiedRGBA
        writeBMP (glitchedFileName n) glitchedBMP) [1..8]
    -- B.writeFile glitchedFileName $ replaceBitsInByteString imageFile textFile 8
    putStrLn "converted"
