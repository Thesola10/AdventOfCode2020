import Data.List(findIndices)

-----

splitOn a b = splitAt ((findIndices (== a) b)!!0) b

getInputs :: IO [(Integer, Integer, Char, String)]
getInputs = do
            content <- readFile "2.in"
            let ns = map parseInput (lines content)
            return ns

parseInput :: String -> (Integer, Integer, Char, String)
parseInput line = do
                let pol = splitOn ':' line
                let range = map toInteger (splitOn '-' (words (pol !! 0) !! 0))
                return (range !! 0, range !! 1, head (words (pol !! 0) !! 0), pol !! 1)

main :: IO ()
main = error "No entry point defined."
