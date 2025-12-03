module Day1 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

data Rotation = L Int | R Int
  deriving Show

turn :: Int -> Rotation -> Int
turn x (L n) = (x - n) `mod` 100
turn x (R n) = (x + n) `mod` 100

parse :: String -> Maybe Rotation
parse s =
    case s of
      ('L':n) -> L <$> readMaybe n
      ('R':n) -> R <$> readMaybe n
      _       -> Nothing

solvePart1 :: FilePath -> IO Int
solvePart1 = fmap (countZeros . lines) . readFile
  where
    countZeros =
        snd . foldl step (50, 0)
    step (cur, acc) str =
        case parse str of
          Just rot ->
            let cur' = turn cur rot
                acc' = if cur' == 0 then acc + 1 else acc
            in (cur', acc')
          Nothing -> (cur, acc)

-------------------------------------------------------------------------------

zeros :: (Int, Int) -> Rotation -> (Int, Int)
zeros (count, pos) rot =
  case rot of
    R n ->
      let distToZero = (100 - pos) `mod` 100
          nextZero   = if distToZero == 0 then 100 else distToZero
          zeroHits   = if nextZero > n then 0 else 1 + (n - nextZero) `div` 100
          pos'       = turn pos rot
      in (count + zeroHits, pos')

    L n ->
      let distToZero = pos `mod` 100
          nextZero   = if distToZero == 0 then 100 else distToZero
          zeroHits   = if nextZero > n then 0 else 1 + (n - nextZero) `div` 100
          pos'       = turn pos rot
      in (count + zeroHits, pos')

solvePart2 :: FilePath -> IO Int
solvePart2 =
  fmap
    ( fst
    . foldl zeros (0, 50)
    . mapMaybe parse
    . lines
    )
  . readFile
