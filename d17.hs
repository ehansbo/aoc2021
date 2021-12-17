import DayZero 
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data Area = Area {minX :: Int, maxX :: Int, minY :: Int, maxY :: Int}
    deriving (Show)

type Velocity = (Int, Int)

main :: IO ()
main = do
    target <- parse <$> readFile "d17.txt"
    print $ solve target
    print $ solve2 target

solve2 :: Area -> Int
solve2 area = length $ mapMaybe (solveForVelocity area) [(x', y') | x' <- [0..100], y' <- [-200..1000]]

solve :: Area -> Int
solve area = maximum $ mapMaybe (solveForVelocity area) [(x', y') | x' <- [0..100], y' <- [0..1000]]

solveForVelocity :: Area -> Velocity -> Maybe Int
solveForVelocity a v = solveForVelocity' v (0, 0) 0
    where solveForVelocity' (x', y') (x, y) highest
            | isWithin a (x, y) = Just highest
            | isPast a (x, y) = Nothing
            | otherwise = solveForVelocity' (if x' == 0 then 0 else x'-1, y'-1) (x+x', y+y') (max highest y)

isWithin :: Area -> (Int, Int) -> Bool
isWithin a (x, y) = x <= maxX a && x >= minX a && y <= maxY a && y >= minY a

isPast :: Area -> (Int, Int) -> Bool
isPast a (x, y) = x > maxX a || y < minY a

parse :: String -> Area
parse s = 
    let (first:second:[]) = splitOn "," s 
        (minX, maxX) = parse' $ drop 15 first
        (minY, maxY) = parse' $ drop 3 second
        parse' s' =
            let (firstC:sndC:[]) = splitOn ".." s'
                (firstC', sndC') = (read firstC, read sndC)
            in (min firstC' sndC', max firstC' sndC') 
    in Area minX maxX minY maxY
