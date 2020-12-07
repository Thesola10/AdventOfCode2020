import Data.List.Split

--- Ex 1: Count for each group how many questions were answered by anyone
--
-- My approach: This is a simple matter of de-duplication. Basically, we
-- parse out newlines from individual groups, then dedupe those groups, and
-- count how many elements remain (assuming all input follows standard)
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : (filter (/= x) (dedup xs))

countEls :: Eq a => a -> [a] -> Int
countEls c s = length [el | el <- s, el == c]

--- Ex 2: Count for each group how many questions were answered by everyone
--
-- My approach: I split the list of answers into two: the answers, and the
-- number of lines (people). I extract all elements which occur the same number
-- of times as there are lines, and after a dedupe pass, we get the number of
-- answers common to everyone in the group.

countCommon :: String -> Int
countCommon l = length (dedup [el | el <- al, (countEls el al) == ms])
            where al = filter (/= '\n') l
                  ms = length (lines l)

---

getInputs :: IO [String]
getInputs = do
            content <- readFile "6.in"
            return $ splitOn "\n\n" content
            

main :: IO ()
main =  do
        ins <- getInputs
        let sumAny = sum (map length $ map (dedup.(filter (/='\n'))) ins)
        let sumAll = sum (map countCommon ins)
        putStrLn $ "Group answers sum: " ++ (show sumAny)
        putStrLn $ "Group common answers sum: " ++ (show sumAll)
