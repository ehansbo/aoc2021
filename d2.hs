import DayZero
import Data.Char

data Instruction = I Direction Int
    deriving (Show)

data Direction = Forward | Down | Up
    deriving (Show, Read, Eq)


main :: IO ()
main = do
    instructions <- manuallyParse "d2.txt" "\n" parseI
    let (h, d) = foldl solve (0, 0) instructions
    print $ h * d
    let (h', d', _) = foldl solve2 (0, 0, 0) instructions
    print $ h' * d'

parseI :: String -> Instruction
parseI s = I (read d) (read i)
    where ((c:cs):i:[]) = words s
          d = toUpper c : cs

solve :: (Int, Int) -> Instruction -> (Int, Int)
solve (h, d) (I dir i) = case dir of
    Forward -> (h + i, d)
    Up -> (h, d - i)
    Down -> (h, d + i)

solve2 :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
solve2 (h, d, a) (I dir i) = case dir of
    Forward -> (h + i, d + i * a, a)
    Up -> (h, d, a - i)
    Down -> (h, d, a + i)
