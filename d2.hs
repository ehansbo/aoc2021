import DayZero
import Data.Char
import Data.List.Split

data Instruction = I Direction Int
    deriving (Show)

data Direction = Forward | Down | Up
    deriving (Show, Read, Eq)


main :: IO ()
main = do
    instructions <- manuallyParse "d2.txt" "\n" parseI
    print $ solve instructions
    print $ solve2 instructions

parseI :: String -> Instruction
parseI s = I (read d') (read i)
    where ((c:cs):i:[]) = splitOn " " s
          d' = toUpper c : cs


solve :: [Instruction] -> Int
solve = solve' (0, 0)
    where solve' (h, d) ((I dir i):is)
            | dir == Forward = solve' (h + i, d) is
            | dir == Up = solve' (h, d - i) is
            | dir == Down = solve' (h, d + i) is
          solve' (h, d) _ = h*d

solve2 = solve' (0, 0, 0)
    where solve' (h, d, a) ((I dir i):is)
            | dir == Forward = solve' (h + i, d + i * a, a) is
            | dir == Up = solve' (h, d, a - i) is
            | dir == Down = solve' (h, d, a + i) is
          solve' (h, d, _) _ = h * d