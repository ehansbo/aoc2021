{-# LANGUAGE ScopedTypeVariables #-}
import DayZero
import Control.Monad.State
import qualified Data.Map as M

data Octopus = Ready Int | Flashed
    deriving (Show)
type Coord = (Int, Int)
type OMap = M.Map (Int, Int) Octopus
type OState = State OMap

main :: IO ()
main = do
    input <- splitFile "d11.txt" "\n"
    let parsed :: OMap = M.fromList $ concat $ map (\(y, s) -> zip [(x, y) | x <- [0..]] s) $ zip [0..] ((map . map) (\x -> Ready (read [x])) input)
    print $ evalState (solve 100) parsed
    print $ evalState solve2 parsed

solve2 :: OState Int
solve2 = solve2' 1
    where solve2' tries = do
            incrementAll
            flashes <- recursiveFlash
            reset
            m <- get
            if flashes == M.size m then return tries else solve2' (tries + 1)

solve :: Int -> OState Int
solve 0 = do 
    return 0
solve n = do
    incrementAll
    flashes <- recursiveFlash
    reset
    score <- solve (n-1)
    return $ score + flashes

reset :: OState ()
reset = modify $ M.map reset'
    where reset' Flashed = Ready 0
          reset' o = o

recursiveFlash :: OState Int
recursiveFlash = do
    flashes <- flash
    if flashes > 0 then do
        flashes' <- recursiveFlash
        return $ flashes + flashes'
    else return 0

flash :: OState Int
flash = do
    m <- get
    let flashCoords = map fst $ filter (\(c, o) -> shouldFlash o) $ M.toList m
    modify $ M.mapWithKey (\coord oct -> if coord `elem` flashCoords then Flashed else oct)
    incrementNeighbors flashCoords
    return $ length flashCoords

incrementNeighbors :: [(Int, Int)] -> OState ()
incrementNeighbors (c:cs) = do
    increment (neighbors c)
    incrementNeighbors cs
incrementNeighbors [] = return ()

increment :: [(Int, Int)] -> OState [()]
increment = mapM (\coord -> modify $ M.update (\oct -> case oct of 
    (Ready i) -> Just (Ready (i+1))
    Flashed -> Just Flashed) coord)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]

shouldFlash :: Octopus -> Bool
shouldFlash (Ready n) = n > 9
shouldFlash Flashed = False

incrementAll :: OState ()
incrementAll = modify $ M.map (\(Ready i) -> Ready (i+1))



