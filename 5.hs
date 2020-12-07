toSeatId :: (Integer, Integer) -> Integer
toSeatId (y, x) = (y * 8) + x

toCoords :: String -> (Integer, Integer)
toCoords line = (sy, sx)
                where sy = fst $ bsp('F','B') (take 7 line) (0, 127)
                      sx = fst $ bsp('L','R') (drop 7 line) (0, 7)

bsp :: (Char, Char) -> String -> (Integer, Integer) -> (Integer, Integer)
bsp (_, _)   []     (min, max) = (min, max)
bsp (lh, uh) (q:qs) (min, max)
            | q == lh = bsp(lh,uh) qs (min, hfwy min max)
            | q == uh = bsp(lh,uh) qs ((hfwy min max) + 1, max)
            where hfwy m n = (m + n) `div` 2

mySeatId :: [Integer] -> Integer
mySeatId ids = [a | (a,b) <- (zip sids [sids!!0 ..]), (a /= b)]!!0 - 1
        where sids = qsort ids

-----

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = lower ++ [x] ++ upper
            where lower = qsort [el | el <- xs, el < x]
                  upper = qsort [eu | eu <- xs, eu > x]

getInputs :: IO [String]
getInputs = do
            content <- readFile "5.in"
            return (lines content)

main :: IO ()
main =  do
        ins <- getInputs
        let seatIds = map (toSeatId.toCoords) ins
        putStrLn $ "Largest seat ID: " ++ (show $ maximum seatIds)
        putStrLn $ "My seat ID: " ++ (show $ mySeatId seatIds)
