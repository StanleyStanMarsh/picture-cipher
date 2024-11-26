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

-- ��������� ���� � ������ �����
byteToBits :: Word8 -> [Int]
byteToBits byte = [if testBit byte i then 1 else 0 | i <- [7,6..0]]

-- ��������� ������ ������ � ������ �� �������� �����
byteStringToBits :: B.ByteString -> [[Int]]
byteStringToBits bs = map (byteToBits . fromIntegral) (B.unpack bs)

-- �������� i-� ��� � ����� �� �������� ��� (0 ��� 1)
replaceIBit :: Word8 -> Int -> Int -> Word8
replaceIBit byte n bit
    | bit == 1  = setBit byte n
    | bit == 0  = clearBit byte n
    | otherwise = error "Bit must be 0 or 1"

-- ��������� ������ �� ����� ������ k
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = take k xs : chunksOf k (drop k xs)

replaceBitsInByteString :: B.ByteString -> B.ByteString -> Int -> B.ByteString
replaceBitsInByteString original replacing n
    | n < 1 || n > 8 = error "Number of bits to replace must be between 1 and 8"
    | B.length replacing * 8 > B.length original * n = error "Not enough bits in original"
    | otherwise = B.pack $ go (B.unpack original) (chunksOf n replacingBits)
  where
    -- ����������� ����� �� replacing � ������ �����
    replacingBits = concatMap byteToBits (B.unpack replacing)

    -- ����������� ������� ��� ������ ����� � ���������� ������������ ������
    go [] [] = []  -- ��� ����� ����������
    go (o:os) [] = o : go os []  -- ��������� ���������� ����� �� original, ���� ��� ������
    go (o:os) (r:rs) = 
        let modifiedByte = replaceNthBits o r  -- �������� ��������� n ���
        in modifiedByte : go os rs  -- ��������� ���������� ���� � ���������� ������������ �������
    
    
    -- �������� ��������� n ��� ����� �� ����� ���� �� replacing
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
    
    -- ������� ��� ��������� ����� �����
    let glitchedFileName n = mconcat [show n, "_", fileName]
    
    -- �������� �� ��������� �� 1 �� 8
    mapM_ (\n -> do
        let modifiedRGBA = B.reverse $ replaceBitsInByteString (B.reverse rgba) textFile n
        let glitchedBMP = packRGBA32ToBMP width height modifiedRGBA
        writeBMP (glitchedFileName n) glitchedBMP) [1..8]
    -- B.writeFile glitchedFileName $ replaceBitsInByteString imageFile textFile 8
    putStrLn "converted"
