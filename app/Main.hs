module Main (main) where

import Lib (encodeTextToImage, decodeTextsFromImages)
import qualified Data.ByteString as B

main :: IO ()
main = do
    putStrLn "enter Caesar shift: "
    caesarShiftInput <- getLine

    let caesarShift = (read caesarShiftInput :: Int)
    let imageFileName = "ciol2.bmp"
    let textFileName = "bio.txt"

    encodeTextToImage imageFileName textFileName caesarShift

    textFile <- B.readFile textFileName
    decodeTextsFromImages [(show caesarShift) ++ "_" ++ (show x) ++ "_" ++ (show $ B.length textFile) ++ "_" ++ imageFileName | x <- [1..8]]