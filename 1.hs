exlDedup :: [Integer] -> [Integer]
exlDedup [] = []
exlDedup (x:xs) = x : (filter (/= x) (exlDedup xs))

exlScan :: [Integer] -> (Integer, Integer)
exlScan [] = error "Cannot scan an empty list"
exlScan (x:xs) = do
                let axs = filter (== 2020 - x) xs
                if length axs > 0
                    then (x, axs !! 0)
                    else exlScan xs

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
        let nnn = exlDedup ns
        let ex1 = exlScan nnn
        let ex2 = exlScan' nnn
        putStrLn $ "Ex1: " ++ (show ex1)
        putStrLn $ "Ex2: " ++ (show ex2)
