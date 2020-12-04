module Problem03 where

import Data.Maybe
import Control.Monad

type Map = [String]
             --focus   down      right
type Velcro = (String, [String], String -> String)

mkVelcro :: Map -> Velcro
mkVelcro m = (head m, tail m, id)

focus :: Velcro -> Char
focus (line, _, r) = head $ r line

right :: Velcro -> Maybe Velcro
right (line, xs, r) = Just (line, xs, tail . r)

down :: Velcro -> Maybe Velcro
down (_, [], _)    = Nothing
down (_, x:xs, r) = Just (x, xs, r)

run :: (Velcro -> Maybe Velcro) -> String -> Int
run move input =
  length $ filter (=='#') $ map focus foci
  where
    velcro = Just . mkVelcro $ map cycle (lines input)
    (Just foci)  = sequence $ takeWhile isJust $ iterate (>>= move) velcro

moves :: [Velcro -> Maybe Velcro]
moves =
  map dr [
    (1, 1)
  , (1, 3)
  , (1, 5)
  , (1, 7)
  , (2, 1)
  ]
  where
    xs move n = foldl (>=>) pure (replicate n move)
    dr (d, r) = xs down d >=> xs right r

runA :: String -> Int
runA  = run $ moves !! 1

runB :: String -> Int
runB input = product $ map (`run` input) moves

exampleInput :: String
exampleInput =
  unlines ["..##.........##.........##.........##.........##.........##.......",
           "#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..",
           ".#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.",
           "..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#",
           ".#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.",
           "..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....",
           ".#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#",
           ".#........#.#........#.#........#.#........#.#........#.#........#",
           "#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...",
           "#...##....##...##....##...##....##...##....##...##....##...##....#",
           ".#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#"]

main :: IO ()
main = do
  putStrLn "Exemplo"
  print $ runA exampleInput
  print $ runB exampleInput
  input <- readFile "input03"
  putStrLn $ "Input 3 size: " ++ show (length input)
  print $ runA input
  print $ runB input
