module Day3 where

import Data.Char (digitToInt)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

largestJoltage :: T.Text -> Int
largestJoltage = snd . T.foldl' step (-1, 0)
  where
    step (greatestDigit, greatestNumber) c =
        let digit      = digitToInt c
            bestNumber =
                if greatestDigit >= 0
                then max greatestNumber (greatestDigit * 10 + digit)
                else greatestNumber
            bestDigit = max greatestDigit digit
        in (bestDigit, bestNumber)

solvePart1 :: FilePath -> IO Int
solvePart1 =
    fmap (sum . map largestJoltage . T.lines)
        . TIO.readFile

---------------------------------------------------------------------

largestJoltage12 :: T.Text -> Int
largestJoltage12 bank =
  let digits      = T.unpack bank
      removeCount = length digits - 12
      result      = go removeCount [] digits
  in read (take 12 (reverse result))
  where
    go :: Int -> String -> String -> String
    go r stack [] = stack
    go r stack (x:xs)
      | r > 0
      , not (null stack)
      , head stack < x
      = go (r - 1) (tail stack) (x:xs)
      | otherwise
      = go r (x : stack) xs


solvePart2 :: FilePath -> IO Int
solvePart2 =
    fmap (sum . map largestJoltage12 . T.lines)
        . TIO.readFile
