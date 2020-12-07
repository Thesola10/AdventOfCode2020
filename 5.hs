--- Ex1: Find the largest seat ID.
--
-- My approach: Literally treat the steps in the exercise as separate funcs.
-- We decode our binary-space seat refs into coordinates, and then apply the
-- seat ID logic to them.
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

--- Ex2: Find my seat ID, which is the only missing seat ID in a set range.
--
-- My approach: Sort the list, zip it against consecutive values, and find
-- the first mismatch. If there's a missing ID, the sorted list is guaranteed
-- to start lagging behind our indexes.
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
