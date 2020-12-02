module Problem02 where

import Data.List.Split

parse :: String -> (Int, Int, Char, String)
parse xs = (lo, up, head c, pass)
  where
   [lim,c,pass] = words xs
   [lo,up] = map read (splitOn "-" lim) :: [Int]

valid1 :: (Int, Int, Char, String) -> Bool
valid1 (lo, up, c, pass) =
  passLen >= lo && passLen <= up
  where
    passLen = length $ filter (== c) pass

valid2 :: (Int, Int, Char, String) -> Bool
valid2 (i0, i1, c, pass) =
    (pass !! (i0 - 1) == c) /= (pass !! (i1 - 1) == c)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input02"
  putStrLn $ "Input 2 size: " ++ show (length input)
  print . length $ filter valid1 input
  print . length $ filter valid2 input
