module Problem04 where

import Data.List
import Data.Char

fields :: [String]
fields = ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:", "cid:"]
                --field  value
type Passport = [(String,String)]

obligatoryFields :: [String]
obligatoryFields = delete "cid:" fields

hasObligatoryFields :: Passport -> Bool
hasObligatoryFields p = null $ obligatoryFields \\ map fst p

isBetween :: Int -> Int -> Int -> Bool
isBetween x a b =  a <= x && x <= b

validateField :: (String, String) -> Bool
validateField ("byr:",v) =
  let byr = read v :: Int in isBetween byr 1920 2002
validateField ("iyr:",v) =
  let iyr = read v :: Int in isBetween iyr 2010 2020
validateField ("eyr:",v) =
  let eyr = read v :: Int in isBetween eyr 2020 2030
validateField ("hgt:",v) =
  let (hgt0, unit) = span isDigit v
      hgt = read hgt0 :: Int in
      case unit of
        "cm" -> isBetween hgt 150 193
        "in" -> isBetween hgt 59 76
        _    -> False
validateField ("hcl:",v) =
  let validChars = ['0'..'9'] ++ ['a'..'f'] in
    (head v == '#') &&
    (length v == 7) &&
    null (nub (tail v) \\ validChars)
validateField ("ecl:",v) =
  v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
validateField ("pid:",v) =
  length v == 9 && all isDigit v
validateField _ = True

validateFields :: Passport -> Bool
validateFields = all validateField

parseInput :: [String] -> [Passport]
parseInput [] = []
parseInput xs =
  map (splitAt 4) (concatMap words p) : parseInput (drop 1 ps)
  where
    (p,ps) = break null xs

loadBatch :: String -> [Passport]
loadBatch input = parseInput $ lines input

exampleInput :: String
exampleInput = unlines [
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
  , "byr:1937 iyr:2017 cid:147 hgt:183cm"
  , ""
  , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
  , "hcl:#cfa07d byr:1929"
  , ""
  , "hcl:#ae17e1 iyr:2013"
  , "eyr:2024"
  , "ecl:brn pid:760753108 byr:1931"
  , "hgt:179cm"
  , ""
  , "hcl:#cfa07d eyr:2025 pid:166559648"
  , "iyr:2011 ecl:brn hgt:59in"
  ]

run :: (Passport -> Bool) -> String -> Int
run v input = length $ filter v (loadBatch input)

runA :: String -> Int
runA = run hasObligatoryFields

runB :: String -> Int
runB = run (\x -> hasObligatoryFields x && validateFields x)


main :: IO ()
main = do
  putStrLn "Exemplo"
  print $ runA exampleInput
  print $ runB exampleInput
  input <- readFile "input04"
  putStrLn $ "Input 4 size: " ++ show (length input)
  putStrLn "Run A:"
  print $ runA input
  putStrLn "Run B:"
  print $ runB input
