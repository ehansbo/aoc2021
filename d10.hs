import DayZero
import Data.Either
import Data.List
import Data.Maybe

main :: IO ()
main = do
    input <- splitFile "d10.txt" "\n"
    print $ solve input
    print $ solve2 input

solve2 :: [String] -> Int
solve2 xs = 
    let scores = sort $ map score2 $ rights $ map findIllegal xs
    in scores !! (length scores `div` 2)

solve :: [String] -> Int
solve = sum . map score . lefts . map findIllegal

score2 :: String -> Int
score2 = (score2' . reverse)
    where
        score2' [] = 0 
        score2' (c:cs) = 5 * score2' cs + (fromJust $ lookup c $ zip ")]}>" [1..])

score :: Char -> Int
score c = fromJust $ lookup c $ zip ")]}>" [3, 57, 1197, 25137]

findIllegal :: String -> Either Char String
findIllegal = findIllegal' []
    where findIllegal' stack [] = Right stack
          findIllegal' stack (x:xs)
            | x `elem` "([<{" = findIllegal' (opposite x:stack) xs
            | head stack /= x = Left x
            | otherwise       = findIllegal' (tail stack) xs 

opposite :: Char -> Char
opposite c = fromJust $ lookup c (zip "([{<" ")]}>")