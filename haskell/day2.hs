module Day2 where

import Data.List (isInfixOf)

split :: Char -> String -> [String]
split sep = foldr f [[]]
  where
    f c acc@(x:xs)
      | c == sep  = [] : acc
      | otherwise = (c:x) : xs

getRange :: [String] -> (Int, Int)
getRange [a, b] = (read a, read b)
getRange _ = error "the list must have exactly two elements"

rangeExpanded :: [String] -> [String]
rangeExpanded rs =
  let (a, b) = getRange rs
   in map show [a .. b]

isHalfEqual :: String -> Bool
isHalfEqual s =
  let len = length s
      mid = len `div` 2
      (first, second) = splitAt mid s
   in first == second && even len

solvePart1 :: FilePath -> IO Int
solvePart1 =
  fmap
  (   sum
    . map read
    . concatMap
      (   filter isHalfEqual
        . rangeExpanded
        . split '-'
      )
    . split ','
  )
  . readFile

--------------------------------------
  
isWholeRepeated :: Eq a => [a] -> Bool
isWholeRepeated s =
  let ss = init (tail (s ++ s))
   in s `isInfixOf` ss
  
solvePart2 :: FilePath -> IO Int
solvePart2 =
  fmap
  (   sum
    . map read
    . concatMap
      (   filter isWholeRepeated
        . rangeExpanded
        . split '-'
      )
    . split ','
  )
  . readFile
