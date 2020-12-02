module Problem02 where

import Data.List.Split

type ValInfo = (Int, Int, Char, String)

parse :: String -> ValInfo
parse xs = (lo, up, head c, pass)
  where
   [lim,c,pass] = words xs
   [lo,up] = map read (splitOn "-" lim) :: [Int]

valid1 :: ValInfo -> Bool
valid1 (lo, up, c, pass) =
  passLen >= lo && passLen <= up
  where
    passLen = length $ filter (== c) pass

valid2 :: ValInfo -> Bool
valid2 (i0, i1, c, pass) =
    (pass !! (i0 - 1) == c) /= (pass !! (i1 - 1) == c)


runInput :: (ValInfo -> Bool) -> String -> Int
runInput f input  =
  length $ filter f (map parse . lines $ input)

runA,runB :: String -> Int
runA = runInput valid1
runB = runInput valid2

main :: IO ()
main = do
  input <- readFile "input02"
  putStrLn $ "Input 2 size: " ++ show (length input)
  print $ runA input
  print $ runB input
