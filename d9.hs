{-# LANGUAGE ScopedTypeVariables #-}
import DayZero
import Data.List
import qualified Data.Map as M

type CoordMap = M.Map Coord Height
type Coord = (Int, Int)
type Height = Int

main :: IO ()
main = do
    input <- splitFile "d9.txt" "\n"
    let parsed :: CoordMap = M.fromList $ concat $ map (\(y, s) -> zip [(x, y) | x <- [0..]] s) $ zip [0..] ((map . map) (\x -> read [x]) input)
    print $ solve parsed
    print $ solve2 parsed

solve :: CoordMap -> Int
solve cm = sum $ map ((+ 1) . snd) $ getLowPoints cm

solve2 :: CoordMap -> Int
solve2 cm =
    let lowPoints = map fst $ getLowPoints cm
        basins = map (getBasin cm) lowPoints
        lengths = (reverse . sort) $ map length basins
    in  (lengths !! 0) * (lengths !! 1) * (lengths !! 2)

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/=x) xs)

getBasin :: CoordMap -> (Int, Int) -> [(Int, Int)]
getBasin cm p = uniq $ getBasin' cm p p
    where getBasin' cm prev (x, y) =
            let height = lookupM (x, y) cm
                neighbors = filter (\p' -> p' /= prev && lookupM p' cm /= 9 && lookupM p' cm >= height) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
            in  if length neighbors == 0 then [(x, y)] else (x, y) : (concat $ (map (getBasin' cm (x,y)) neighbors))


getLowPoints :: CoordMap -> [(Coord, Height)]
getLowPoints cm = filter (\(c, p) -> isLowPoint c p) (M.toList cm)
    where isLowPoint (x, y) p = fl (x+1, y) p && fl (x-1, y) p && fl (x, y+1) p && fl (x, y-1) p
          fl c p = lookupM c cm > p

lookupM :: Coord -> CoordMap -> Height
lookupM = M.findWithDefault 9