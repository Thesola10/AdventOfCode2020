dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : (filter (/= x) (dedup xs))

--- Ex 1: Find the two expense report elements that add up to 2020
--
-- My approach: A pretty naive scan, where we try to add the first item to 
-- all other elements and see if one adds up to 2020. If nothing is found,
-- try with the second item, etc.
-- Since addition is a reversible operation, if the first pass didn't find
-- anything, we don't need to check the first item in the second pass, so
-- we drop the first item before the next attempt.
exlScan :: [Integer] -> (Integer, Integer)
exlScan [] = error "Cannot scan an empty list"
exlScan (x:xs) = do
                let axs = filter (== 2020 - x) xs
                if length axs > 0
                    then (x, axs !! 0)
                    else exlScan xs

--- Ex 2: Find the three expense report elements that add up to 2020
--
-- My approach: Basically, it's like the scan above, but with dual recursion.
-- For each element x, for each element y, check that x+y+z = 2020
exlScan' :: [Integer] -> Maybe (Integer, Integer, Integer)
exlScan' [] = error "Cannot scan an empty list"
exlScan' (x:xs) =   do
                    let res = exlScan'Sub (x:xs)
                    if res /= Nothing
                        then res
                        else exlScan' xs

exlScan'Sub :: [Integer] -> Maybe (Integer, Integer, Integer)
exlScan'Sub [] = error "Cannot scan an empty list"
exlScan'Sub [x,y] = Nothing
exlScan'Sub (x:y:as) = do
                    let aas = filter (== 2020 - x - y) as
                    if length aas > 0
                        then Just (x, y, aas !! 0)
                        else exlScan'Sub (x:as)

-----

getInputs :: IO [Integer]
getInputs = do
            content <- readFile "1.in"
            let ns = map read (lines content) :: [Integer]
            return ns

main :: IO ()
main =  do
        ns <- getInputs
        let nnn = dedup ns
        let ex1 = exlScan nnn
        let ex2 = exlScan' nnn
        putStrLn $ "Ex1: " ++ (show ex1)
        putStrLn $ "Ex2: " ++ (show ex2)
