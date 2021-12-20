import DayZero
import Data.List.Split
import Data.List
import Debug.Trace


type Beacon = (Integer, Integer, Integer)
data Scanner = Unoriented {getId :: Int, getBeacons :: [Beacon]} | Oriented {getId :: Int, getCoords :: (Integer, Integer, Integer), getBeacons :: [Beacon]}

instance Show Scanner where
    show (Unoriented id _) = show id
    show (Oriented id c _) = show (id, c)

main :: IO ()
main = do
    let testScanner = Unoriented 0 [(0, 2, 1), (4, 1, 0), (3, 3, 1)]
    input <- manuallyParse "d19.txt" "\n\n" parse
    let solution = solve [orient $ head input] (tail input)
    let beacons = nub $ concatMap getAbsoluteCoords solution
    print $ length beacons
    print $ maximum $ map (uncurry manhattan) $ pairsGen solution 

manhattan :: Scanner -> Scanner -> Integer
manhattan (Oriented _ (x1, y1, z1) _) (Oriented _ (x2, y2, z2) _) = abs (x2-x1) + abs (y2-y1) + abs (z2 - z1) 

getAbsoluteCoords :: Scanner -> [Beacon]
getAbsoluteCoords (Oriented _ (x, y, z) beacons) = map (\(x1, y1, z1) -> (x1+x, y1+y, z1+z)) beacons

solve :: [Scanner] -> [Scanner] -> [Scanner]
solve curr [] = curr
solve [] _ = error "could not find all solutions"
solve (curr@(Oriented i c beacons):currs) others = 
    let candidates = getCandidates curr others
    in if length candidates == 0 then curr : solve currs others else
        let chosens = map (orientBy curr) candidates
        in curr : solve (currs ++ chosens) (removeAll chosens others)

getCandidates :: Scanner -> [Scanner] -> [Scanner]
getCandidates curr@(Oriented i c beacons) others = 
    let p = pairs curr
    in filter (isCandidate p) (concatMap rotate others)
        where isCandidate p scanner = 
                let l = (length $ intersectBy (\a b -> fst a == fst b) (pairs scanner) p)
                in l >= 12 

orient :: Scanner -> Scanner
orient sc = Oriented (getId sc) (0, 0, 0) (getBeacons sc)

orientBy :: Scanner -> Scanner -> Scanner
orientBy sc1@(Oriented _ (x, y, z) _) sc2@(Unoriented i beacons') =
    let pairs1 = sort $ pairs sc1
        pairs2 = sort $ pairs sc2
        ((x1, y1, z1), (x2, y2, z2)) = findNMatch 1 pairs1 pairs2
    in Oriented i (x+x1-x2, y+y1-y2, z+z1-z2) beacons'

removeAll :: [Scanner] -> [Scanner] -> [Scanner]
removeAll (s:ss) scanners = removeAll ss (remove s scanners)
removeAll _ scanners = scanners

remove :: Scanner -> [Scanner] -> [Scanner]
remove scanner scanners = filter (\sc -> getId sc /= getId scanner) scanners


findNMatch :: Int -> [(Integer, (Beacon, Beacon))] -> [(Integer, (Beacon, Beacon))] -> (Beacon, Beacon)
findNMatch n all1@((i1, (b1, b1')):xs1) all2@((i2, (b2, b2')):xs2)
    | i1 == i2 && n == 1 = (b1, b2)
    | i1 == i2 = findNMatch (n-1) xs1 xs2
    | i1 < i2  = findNMatch n xs1 all2
    | otherwise = findNMatch n all1 xs2


parse :: String -> Scanner
parse s = 
    let filtered = filter (/= []) $ splitOn "\n" s
    in Unoriented (parse'' $ head filtered) $ map parse' $ tail filtered
        where parse' s = let (x:y:z:[]) = splitOn "," s in (read x, read y, read z)
              parse'' s = read $ (splitOn " " s) !! 2

pairs :: Scanner -> [(Integer, (Beacon, Beacon))]
pairs l = [(relativeDistance x y, (x, y)) | (x:ys) <- tails (getBeacons l), y <- ys]

pairsGen :: [a] -> [(a, a)]
pairsGen l = [(x, y) | (x:ys) <- tails l, y <- ys]

relativeDistance :: Beacon -> Beacon -> Integer
relativeDistance (x1, y1, z1) (x2, y2, z2) = 10000*10000*(x2-x1) + 10000*(y2-y1) + z2-z1

rotate :: Scanner -> [Scanner]
rotate (Unoriented i bs) = 
    let rots = map rotations bs
    in rotate' rots
        where rotate' xs
                | (length . head) xs > 0 = Unoriented i (map head xs) : rotate' (map tail xs)
                | otherwise = []

rotations :: (Integer, Integer, Integer) -> [(Integer, Integer, Integer)]
rotations (x, y, z) = [
    (x, y, z), 
    (x, -y, -z), 
    (-x, y, -z), 
    (-x, -y, z), 
    (x, z, -y), 
    (x, -z, y), 
    (-x, z, y), 
    (-x, -z, -y), 
    (y, z, x), 
    (y, -z, -x), 
    (-y, z, -x), 
    (-y, -z, x),
    (y, x, -z), 
    (y, -x, z), 
    (-y, x, z), 
    (-y, -x, -z), 
    (z, x, y), 
    (z, -x, -y), 
    (-z, x, -y), 
    (-z, -x, y), 
    (z, y, -x), 
    (z, -y, x), 
    (-z, y, x), 
    (-z, -y, -x)]