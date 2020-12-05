scanDiagonal :: Int -> Int -> [String] -> Integer
scanDiagonal dy dx table = let nthl = [l | (n,l) <- (zip [0..] table), n `mod` dy == 0]
                        in sum [if tl!!(dx*n `mod` length tl) == '#' then 1 else 0 | (n, tl) <- (zip [0..] nthl)]

-----

getInputs :: IO [String]
getInputs = do
            content <- readFile "3.in"
            return (lines content)

main :: IO ()
main =  do
        ins <- getInputs
        putStrLn $ "Trees hit: " ++ (show (scanDiagonal 1 3 ins))
        let mulSlopes = (scanDiagonal 1 1 ins)
                        * (scanDiagonal 1 3 ins)
                        * (scanDiagonal 1 5 ins)
                        * (scanDiagonal 1 7 ins)
                        * (scanDiagonal 2 1 ins)
        putStrLn $ "Product of tested slopes: " ++ (show mulSlopes)
