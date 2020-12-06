module Problem06 where

import Data.List.Split ( splitOn )
import Data.Set (intersection, fromList)

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
run f input = sum $ map f (splitOn [""] $ lines input)

runA :: String -> Int
runA = run $ length . fromList . concat

runB :: String -> Int
runB = run $ length . foldl1 intersection . map fromList

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
