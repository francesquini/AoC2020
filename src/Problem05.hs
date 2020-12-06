module Problem05 where

exampleInputs :: [(String, Int)]
exampleInputs = [
    ("FBFBBFFRLR", 357)
  , ("BFFFBBFRRR", 567)
  , ("FFFBBBFRRR",  119)
  , ("BBFFBBFRLL", 820)
  ]


parseInput :: String -> Int
parseInput xs =
  foldl (\acc d -> acc * 2 + d) 0 $ map toInt xs
  where
    toInt c = if c `elem` "BR" then 1 else 0

run :: String -> [Int]
run input = map parseInput (lines input)

runA :: String -> Int
runA input = maximum $ run input

runB :: String -> Int
runB input = total - sum xs
  where
    xs = run input
    (mn, mx) = (minimum xs, maximum xs)
    total = (mx + mn) * (mx - mn + 1) `div` 2

main :: IO ()
main = do
  putStrLn "Exemplos"
  print $ runA (unlines $ map fst exampleInputs)
  input <- readFile "input05"
  putStrLn $ "Input 5 size: " ++ show (length input)
  putStrLn "Run A:"
  print $ runA input
  putStrLn "Run B:"
  print $ runB input
