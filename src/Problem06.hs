module Problem06 where

import Data.List
import Data.List.Split

exampleInput :: String
exampleInput = unlines [
    "abc"
  , ""
  , "a"
  , "b"
  , "c"
  , ""
  , "ab"
  , "ac"
  , ""
  , "a"
  , "a"
  , "a"
  , "a"
  , ""
  , "b"
  ]

run :: ([String] -> Int) -> String -> Int
run f input =
  sum $ map f (splitOn [""] $ lines input)

runA :: String -> Int
runA = run $ length . nub . concat

runB :: String -> Int
runB = run $ length . foldl intersect ['a'..'z']

main :: IO ()
main = do
  putStrLn "Exemplos"
  print $ runA exampleInput
  input <- readFile "input06"
  putStrLn $ "Input 6 size: " ++ show (length input)
  putStrLn "Run A:"
  print $ runA input
  putStrLn "Run B:"
  print $ runB input
