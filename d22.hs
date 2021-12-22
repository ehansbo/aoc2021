import DayZero
import Data.List.Split
import Data.Maybe

data Cube = Cube {xMin :: Int, xMax :: Int, yMin :: Int, yMax :: Int, zMin :: Int, zMax :: Int, on :: Bool}
    deriving (Show)

main :: IO ()
main = do
    input <- manuallyParse "d22.txt" "\n" parse
    let small = filter (\c -> xMin c >= (-50) && xMax c <= 50) input
    print $ sum $ map volume $ solve small []
    print $ sum $ map volume $ solve input []

solve :: [Cube] -> [Cube] -> [Cube]
solve [] prev = prev
solve (c:cs) prev 
    | not $ on c = solve cs $ (getIntersects c prev) ++ prev
    | on c = solve cs $ c : ((getIntersects c prev) ++ prev)

getIntersects :: Cube -> [Cube] -> [Cube]
getIntersects c cs = map fromJust $ filter isJust $ map (intersect c) cs

volume :: Cube -> Integer
volume (Cube xMin xMax yMin yMax zMin zMax b) = fromIntegral (xMax - xMin + 1) * fromIntegral (yMax - yMin + 1) * fromIntegral (zMax - zMin + 1) * (if b then 1 else -1)

intersect :: Cube -> Cube -> Maybe Cube
intersect (Cube xMin1 xMax1 yMin1 yMax1 zMin1 zMax1 b1) (Cube xMin2 xMax2 yMin2 yMax2 zMin2 zMax2 b2) =
    let (xMin, xMax, yMin, yMax, zMin, zMax) = (max xMin1 xMin2, min xMax1 xMax2, max yMin1 yMin2, min yMax1 yMax2, max zMin1 zMin2, min zMax1 zMax2)
    in if xMin <= xMax && yMin <= yMax && zMin <= zMax then Just $ Cube xMin xMax yMin yMax zMin zMax (getLight b1 b2) else Nothing

getLight :: Bool -> Bool -> Bool
getLight b1 b2 = b1 && not b2 || not b1 && not b2


parse :: String -> Cube
parse s = 
    let (fst:xs:ys:zs:[]) = splitOn "=" s
        getCoord ss = 
            let sp = splitOn ".." ss
                minS = sp !! 0
                maxS = filter (`elem` '-':['0'..'9']) (sp !! 1)
            in (read minS, read maxS)
        on = 'n' == (fst !! 1)
        (xMin, xMax) = getCoord xs
        (yMin, yMax) = getCoord ys
        (zMin, zMax) = getCoord zs
    in Cube xMin xMax yMin yMax zMin zMax on