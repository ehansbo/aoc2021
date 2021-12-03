import DayZero
import Data.Bits

main :: IO ()
main = do
    input <- splitFile "d3.txt" "\n"
    print $ solve input
    print $ solve2 input


zipList :: [[a]] -> [[a]]
zipList ([]:_) = []
zipList xs = (map head xs) : zipList (map tail xs)

toGamma :: String -> Char
toGamma s 
    | length (filter (== '1') s) >= (length s) `div` 2 = '1'
    | otherwise = '0'

solve :: [String] -> Int
solve input =     
    let zipped = zipList input
        bits = length $ head input
        gamma = map toGamma zipped
        swap '1' = '0'
        swap '0' = '1'
        epsilon = map swap gamma
    in (toInt gamma) * (toInt epsilon)

solve2 :: [String] -> Int
solve2 input = 
    let 
        oxygen = findBy (\pos -> if length (filter (== '1') pos) >= length (filter (== '0') pos) then '1' else '0') input
        co2 = findBy (\pos -> if length (filter (== '0') pos) <= length (filter (== '1') pos) then '0' else '1') input
    in (toInt oxygen) * (toInt co2)

findBy :: (String -> Char) -> [String] -> String
findBy = findBy' 0
    where findBy' _ _ (x:[]) = x
          findBy' i f xs =
            let pos = map (!! i) xs
                filterChar = f pos
            in findBy' (i+1) f (filter (\x -> x !! i == filterChar) xs)

toInt :: String -> Int
toInt [] = 0
toInt ('0':xs) = toInt xs
toInt ('1':xs) = 2^(length xs) + toInt xs 

