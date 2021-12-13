import DayZero
import Data.List.Split
import qualified Data.Set as S

data Fold = FoldY Int | FoldX Int
    deriving (Show)
type Coord = (Int, Int)


main :: IO ()
main = do
    (i1:i2:[]) <- splitFile "d13.txt" "\n\n" 
    let coords = S.fromList $ map (\s -> let (c1:c2:[]) = splitOn "," s in (read c1, read c2)) $ splitOn "\n" i1
    let folds = map parseFold $ filter (/= "") $ splitOn "\n" i2
    print $ S.size $ fold coords $ head folds
    putStr $ toString $ foldl fold coords folds

fold :: S.Set Coord -> Fold -> S.Set Coord
fold coords f = S.map (fold' f) coords
    where fold' (FoldY line) (x, y)
            | y < line = (x, y)
            | y > line = (x, 2*line-y)
          fold' (FoldX line) (x, y)
            | x < line = (x, y)
            | x > line = (2*line-x, y)

parseFold :: String -> Fold
parseFold s = 
    let relevant = last $ words s
        (c:i:[]) = splitOn "=" relevant
        coord = read i
    in if c == "y" then FoldY coord else FoldX coord

toString :: S.Set Coord -> String
toString coords = 
    let maxX = S.findMax $ S.map fst coords
        maxY = S.findMax $ S.map snd coords
        showCoord coord = if coord `S.member` coords then '#' else ' '
    in unlines $ map (\y -> [showCoord (x, y) | x <- [0..maxX]]) [0..maxY]