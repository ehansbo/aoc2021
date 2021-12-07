import DayZero

type Pos = Int

main :: IO ()
main = do
    start <- splitAndReadFile "d7.txt" ","
    let possibleStops = [minimum start..maximum start]
    print $ minimum $ map (solveFor start) possibleStops
    print $ minimum $ map (solveFor2 start) possibleStops

solveFor :: [Pos] -> Pos -> Int
solveFor (x:xs) end = abs (x - end) + solveFor xs end
solveFor _ _ = 0

solveFor2 :: [Pos] -> Pos -> Int
solveFor2 (x:xs) end = let n = abs (x - end) in n * (n + 1) `div` 2 + solveFor2 xs end
solveFor2 _ _ = 0