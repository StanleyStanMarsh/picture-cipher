module Lib
    ( encodeTextToImage
    , decodeTextsFromImages
    ) where

import qualified Data.Text as T
import Data.Char (ord, chr)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Data.Bits (testBit, setBit, clearBit)
import Data.Word (Word8)
import Codec.BMP

caesarCipher :: T.Text -> Int -> T.Text
caesarCipher str key = T.map shiftChar str
  where
    shiftChar c
      | c >= 'a' && c <= 'z' = chr $ (ord c - ord 'a' + key) `mod` 26 + ord 'a'
      | c >= 'A' && c <= 'Z' = chr $ (ord c - ord 'A' + key) `mod` 26 + ord 'A'
      | otherwise = c

caesarDecipher :: T.Text -> Int -> T.Text
caesarDecipher str key = caesarCipher str (-key)

-- переводит байт в список битов
byteToBits :: Word8 -> [Int]
byteToBits byte = [if testBit byte i then 1 else 0 | i <- [7,6..0]]

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
        foldl (\acc (bit, idx) -> replaceIBit acc idx bit) byte (zip newBits (reverse [0..n-1]))

encodeTextToImage :: FilePath -> FilePath -> Int -> IO ()
encodeTextToImage imageFileName textFileName caesarK = do
    Right bmp <- readBMP imageFileName
    let rgba = unpackBMPToRGBA32 bmp
    let (width, height) = bmpDimensions bmp
    textFile <- B.readFile textFileName
    
    -- Функция для генерации имени файла
    let glitchedFileName n = (show caesarK) ++ "_" ++ (show n) ++ "_" ++ (show $ B.length textFile) ++ "_" ++ imageFileName

    let codedTextFile = TE.encodeUtf8 $ caesarCipher (TE.decodeUtf8 textFile) caesarK

    B.writeFile ("coded_" ++ textFileName) codedTextFile
    
    -- Итерация по значениям от 1 до 8
    mapM_ (\n -> do
        let modifiedRGBA = B.reverse $ replaceBitsInByteString (B.reverse rgba) (codedTextFile) n
        let glitchedBMP = packRGBA32ToBMP width height modifiedRGBA
        -- print $ B.take 10 $ B.reverse modifiedRGBA
        writeBMP (glitchedFileName n) glitchedBMP) [1..8]
    -- B.writeFile glitchedFileName $ replaceBitsInByteString imageFile textFile 8
    putStrLn "converted"

-- Извлечение последних n бит из каждого байта
extractBitsFromByte :: Word8 -> Int -> [Int]
extractBitsFromByte byte n = reverse [if testBit byte i then 1 else 0 | i <- [0..n-1]]

bitsToBytes :: [Int] -> B.ByteString
bitsToBytes [] = B.empty
bitsToBytes bits = 
    B.cons (fromIntegral (foldl (\acc (b, i) -> if b == 1 then setBit acc i else acc) (0 :: Word8) (zip (take 8 bits) [7,6..0])))
           (bitsToBytes (drop 8 bits))

-- Восстановление текста из изображения
decodeTextFromImage :: FilePath -> IO ()
decodeTextFromImage imageFileName = do
    -- Извлекаем параметры из имени файла
    let [caesarKStr, nStr, textLenStr, _] = T.splitOn (T.pack "_") (T.pack imageFileName)
    let caesarK = read (T.unpack caesarKStr) :: Int
    let n = read (T.unpack nStr) :: Int
    let textLen = read (T.unpack textLenStr) :: Int
    
    -- Читаем изображение
    Right bmp <- readBMP imageFileName
    let rgba = B.reverse $ unpackBMPToRGBA32 bmp

    -- Извлекаем последние n бит из каждого байта
    let extractedBits = concatMap (`extractBitsFromByte` n) (B.unpack rgba)
    
    -- Берем нужное количество бит и преобразуем в байты
    let textBytes = bitsToBytes $ take (textLen * 8) extractedBits
    
    -- Декодируем байты в текст
    let decodedText = caesarDecipher (TE.decodeUtf8 textBytes) caesarK
    
    -- Записываем восстановленный текст в файл
    let outputFileName = imageFileName ++ "_decoded.txt"
    B.writeFile outputFileName $ TE.encodeUtf8 decodedText
    putStrLn $ "Decoded text written to " ++ outputFileName

-- Декодирование текста из всех изображений
decodeTextsFromImages :: [FilePath] -> IO ()
decodeTextsFromImages imageFileNames = mapM_ decodeTextFromImage imageFileNames