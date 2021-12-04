import DayZero
import Data.List.Split

type Bingo = [[(Int, Bool)]]

main :: IO ()
main = do
    (movesStr:bingoStrs) <- splitFile "d4.txt" "\n\n"
    let moves = map read (splitOn "," movesStr)
    let bingos = map parseBingo bingoStrs
    print $ solve bingos moves
    print $ solve2 bingos moves


solve :: [Bingo] -> [Int] -> Int
solve bingos moves = getScore $ winner bingos moves

solve2 :: [Bingo] -> [Int] -> Int
solve2 bingos moves = getScore $ loser bingos moves

loser :: [Bingo] -> [Int] -> (Bingo, Int)
loser bingos (move:moves) =
    let updated = (map . map . map) (applyMove move) bingos
        maybeWinners = filter isWinner updated
    in  if length maybeWinners == 1 && length bingos == 1
        then (head maybeWinners, move)
        else loser (filter (not . isWinner) updated) moves

winner :: [Bingo] -> [Int] -> (Bingo, Int)
winner bingos (move:moves) =
    let updated = (map . map . map) (applyMove move) bingos
        maybeWinners = filter isWinner updated
    in  if length maybeWinners == 1 
        then (head maybeWinners, move) 
        else winner updated moves

applyMove :: Int -> (Int, Bool) -> (Int, Bool)
applyMove move (i, b)
    | i == move = (i, True)
    | otherwise = (i, b)

getScore :: (Bingo, Int) -> Int
getScore (bingo, move) = move * (sum $ map fst (filter (\(i, b) -> not b) (concat bingo)))

parseBingo :: String -> Bingo
parseBingo = (map . map) ((\i -> (i, False)) . read) . map words . lines

isWinner :: Bingo -> Bool
isWinner b = winnerRow b || winnerColumn b

winnerRow :: Bingo -> Bool
winnerRow [] = False
winnerRow (row:rows) = 0 == (length $ filter (\(_, b) -> not b) row) || winnerRow rows 

winnerColumn :: Bingo -> Bool
winnerColumn ([]:_) = False
winnerColumn rows = 
    let (col, cols) = (map head rows, map tail rows)
    in 0 == (length $ filter (\(_, b) -> not b) col) || winnerColumn cols
