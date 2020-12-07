import Data.List
import Data.List.Split
import Data.Maybe
import Text.Regex.TDFA

checkFields :: [(String, String)] -> Bool
checkFields pass =  do
                    let missingKeys = [field | field <- keys, isNothing $ elemIndex field (map fst pass)]
                    length missingKeys == 0
                where keys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

checkFields' :: [(String, String)] -> Bool
checkFields' pass = do
                    let invalidKeys = filter (not . validField) pass
                    (checkFields pass) && (length invalidKeys == 0)

validField :: (String, String) -> Bool
validField (key, value)
          | key == "byr"  = (read value :: Integer) <? (1920, 2002)
          | key == "iyr"  = (read value :: Integer) <? (2010, 2020)
          | key == "eyr"  = (read value :: Integer) <? (2020, 2030)
          | key == "hgt"  = validHeight value
          | key == "hcl"  = value =~ "^#[0-9a-f]{6}$"
          | key == "ecl"  = not (isNothing $ elemIndex value eyeColors)
          | key == "pid"  = value =~ "^[0-9]{9}$"
          | otherwise     = True
          where eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validHeight :: String -> Bool
validHeight entry
          | length fmt == 0   = False
          | fmt!!0!!2 == "in" = (read (fmt!!0!!1) :: Integer) <? (59, 76)
          | fmt!!0!!2 == "cm" = (read (fmt!!0!!1) :: Integer) <? (150, 193)
          where fmt = entry =~ "^([0-9]+)(in|cm)$" :: [[String]]

-----

-- pretty range function pulled from StackOverflow
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

getInputs :: IO [[(String, String)]]
getInputs = do
            content <- readFile "4.in"
            return (map parseInput (splitOn "\n\n" content))

parseInput :: String -> [(String, String)]
parseInput s = [ ((splitOn ":" fi)!!0, (splitOn ":" fi)!!1) | fi <- (words s) ]

main :: IO ()
main =  do
        ins <- getInputs
        putStrLn $ "Valid passports: " ++ (show $ length [x | x <- (map checkFields ins), x == True])
        putStrLn $ "Valid passports (new): " ++ (show $ length [y | y <- (map checkFields' ins), y == True])
