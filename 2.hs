import Text.Regex.TDFA

countEls :: Eq a => a -> [a] -> Integer
countEls c s = sum [1 | el <- s, el == c]

--- Ex 1: Count how many passwords fail the first given policy
--
-- My approach: We take the specified character, count how many of it are
-- in the password, and see if it fits in the range.
-- Most of the work is already done by the input parser, which itself is
-- built around a regular expression.
checkPolicy :: (Integer, Integer, Char, String) -> Bool
checkPolicy (min, max, which, pass) = 
                    do
                    let nchar = countEls which pass
                    (nchar >= min) && (nchar <= max)

countFails :: [(Integer, Integer, Char, String)] -> Integer
countFails lines = sum [if checkPolicy a then 0 else 1 | a <- lines]

--- Ex 2: Count how many passwords pass the second given policy
--
-- My approach: Again, once a password line is parsed, all we need is to check
-- the given indexes for the letter and XOR the results together (exactly one)
checkPolicy' :: (Integer, Integer, Char, String) -> Bool
checkPolicy' (i1, i2, which, pass) = 
             ((pass!!(fromIntegral i1 - 1)) == which) /= ((pass!!(fromIntegral i2 - 1)) == which)

countFails' :: [(Integer, Integer, Char, String)] -> Integer
countFails' lines = sum [if checkPolicy' a then 1 else 0 | a <- lines]

-----

getInputs :: IO [(Integer, Integer, Char, String)]
getInputs = do
            content <- readFile "2.in"
            let ns = map parseInput (lines content)
            return ns

parseInput :: String -> (Integer, Integer, Char, String)
parseInput line = 
            do
            let res = (line =~ "^([0-9]{1,2})-([0-9]{1,2}) ([a-z]): (.+)$" :: [[String]]) !! 0
            (read (res!!1) :: Integer, read (res!!2) :: Integer, res!!3!!0, res!!4)

main :: IO ()
main =  do
        dat <- getInputs
        putStrLn $ "Failing passwords: " ++ (show (countFails dat))
        putStrLn $ "Good passwords (new): " ++ (show (countFails' dat))
