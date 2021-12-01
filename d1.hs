import DayZero

main :: IO ()
main = do
    depth <- splitAndReadFile "d1.txt" "\n" 
    putStrLn $ (show . sumDepth) depth
    putStrLn $ (show . sumDepth . window) depth

sumDepth :: [Int] -> Int
sumDepth (x:xs@(y:_))
    | y > x = 1 + sumDepth xs
    | otherwise = sumDepth xs
sumDepth _ = 0

window :: [Int] -> [Int]
window (x:xs@(y:z:_)) = (x + y + z) : window xs
window _ = []
