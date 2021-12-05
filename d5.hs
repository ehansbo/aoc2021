import DayZero
import Data.List.Split
import qualified Data.Map as M

type Point = (Int, Int)
type Line = (Point, Point)
type PMap = M.Map Point Int


main :: IO ()
main = do
    lines <- manuallyParse "d5.txt" "\n" pLine
    print $ solve lines
    print $ solve2 lines

solve :: [Line] -> Int
solve lines = let pMap = getMap (filter straight lines) in M.size $ M.filter (> 1) pMap

solve2 :: [Line] -> Int
solve2 lines = let pMap = getMap lines in M.size $ M.filter (> 1) pMap

straight :: Line -> Bool
straight ((x1,y1), (x2, y2)) = x1 == x2 || y1 == y2

getMap :: [Line] -> PMap
getMap = getMap' M.empty
    where getMap' pMap (l:ls) = getMap' (addToMap l pMap) ls
          getMap' pMap _ = pMap

addToMap :: Line -> PMap -> PMap
addToMap l pMap = foldl (\nMap p -> M.insertWith (+) p 1 nMap) pMap (points l)

points :: Line -> [Point]
points ((x1, y1), (x2, y2))
    | x1 == x2 = [(x1, y) | y <- [(min y1 y2)..(max y1 y2)]]
    | y1 == y2 = [(x, y1) | x <- [(min x1 x2)..(max x1 x2)]]
    | otherwise = zip [x1,(if x2 > x1 then x1+1 else x1-1)..x2] [y1,(if y2 > y1 then y1+1 else y1-1)..y2]

pLine :: String -> Line
pLine s = 
    let (p1str:p2str:[]) = splitOn " -> " s
        r pstr = let (i1s:i2s:[]) = splitOn "," pstr in (read i1s, read i2s)
    in (r p1str, r p2str)