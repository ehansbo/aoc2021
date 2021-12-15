import DayZero
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import Control.Monad.State

type Coord = (Int, Int)
type QState = State ((Q.MinQueue (Int, Coord)), S.Set Coord)


main :: IO ()
main = do
    input <- splitFile "d15.txt" "\n"
    let grid = (map . map) (\c -> read [c]) input
    let (lx, ly) = (length (head grid), length grid)
    let solution mx my = (evalState (solve mx my grid) $ (Q.singleton (0, (0, 0)), S.empty))
    print $ solution (lx - 1) (ly - 1)
    let maxX2 = 5*lx - 1
    let maxY2 = 5*ly - 1
    print $ solution maxX2 maxY2

solve :: Int -> Int -> [[Int]] -> QState Int
solve maxX maxY grid = do
    (l, (x, y)) <- pop
    visited <- isVisited (x, y)
    if visited then solve maxX maxY grid else do
        let neighbors = map (\n -> (n, getPoint grid n)) $ getNeighbors (x, y) (maxX, maxY)
        let handleCoord 
              | maxX == x && maxY == y = return l
              | otherwise = do 
                    addVisited (x, y)
                    sequence $ map (\(n, p) -> push (l+p, n)) neighbors
                    solve maxX maxY grid
        handleCoord

getPoint :: [[Int]] -> Coord -> Int
getPoint grid (x, y) = 
    let (lx, ly) = (length (head grid), length grid)
        addition = (x `div` lx + y `div` ly)
        result = addition + (grid !! (y `mod` ly) !! (x `mod` lx))
    in ((result-1) `mod` 9) + 1

isVisited :: Coord -> QState Bool
isVisited c = do
    (_, visited) <- get
    return $ S.member c visited

addVisited :: Coord -> QState ()
addVisited coords = do
    (queue, visited) <- get
    let visited' = S.insert coords visited
    put (queue, visited')

getNeighbors :: Coord -> Coord -> [Coord]
getNeighbors (x, y) (maxX, maxY)= filter (\(x', y') -> x' <= maxX && y' <= maxY) [(x+1, y), (x, y+1)]

pop :: QState (Int, Coord)
pop = do
    (queue, visited) <- get
    let (result, newQueue) = Q.deleteFindMin queue
    put (newQueue, visited)
    return result

push :: (Int, Coord) -> QState ()
push d = do
    (queue, visited) <- get
    let queue' = Q.insert d queue
    put (queue', visited)
