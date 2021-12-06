import DayZero
import Data.List
type Fishes = (Age, Int)
type Age = Int


main :: IO ()
main = do
    fish <- splitAndReadFile "d6.txt" ","
    let clustered = initiate fish
    print $ solve 80 clustered
    print $  solve 256 clustered

initiate :: [Int] -> [Fishes]
initiate xs = map (\x -> (head x, length x)) $ groupBy (==) $ sort xs

solve :: Int -> [Fishes] -> Int
solve n fish = sum $ map snd $ ((iterate solve') fish) !! n
    where solve' fish = merge $ concat $ map (\(age, num) -> if age == 0 then [(6, num), (8, num)] else [(age-1, num)]) fish

merge :: [Fishes] -> [Fishes]
merge fishes = map (\xs -> (foldl1 (\(age, num1) (age2, num2) -> (age, num1 + num2)) xs)) $ groupBy (\(a1, _) (a2, _) -> (a1 == a2)) $ sort fishes