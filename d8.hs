import DayZero
import Data.List.Split
import Data.List
import Control.Monad.State
import qualified Data.Map as M
import System.IO.Unsafe
import Data.Maybe

type MState = State (M.Map Int String)

main :: IO ()
main = do
    input <- splitFile "d8.txt" "\n"
    print $ solve input
    let input2 = map parse input
    let solution2 = map (\s -> evalState (solve2 s) M.empty) input2
    print $ sum solution2
    
parse :: String -> ([String], [String])
parse s = 
    let splitted = splitOn " | " s
        f s = map sort $ splitOn " " s
    in (f $ splitted !! 0, f $ splitted !! 1)

solve2 :: ([String], [String]) -> MState Int
solve2 (q, a) = do
    calculateMap q
    m <- get
    let reverseM = M.fromList $ map (\(k, v) -> (v, k)) (M.toList m)
    let numbers = map (\x -> fromJust $ M.lookup x reverseM) a
    return $ (numbers !! 0) * 1000 + (numbers !! 1) * 100 + (numbers !! 2) * 10 + (numbers !! 3)    

updateM :: Int -> String -> MState ()
updateM k v = do
    m <- get
    put $ M.insert k v m
    return ()

lookupM :: Int -> MState String
lookupM k = do
    m <- get
    let (Just s) = m M.!? k
    return s


solve :: [String] -> Int
solve ss = (solve' 2) + (solve' 3) + (solve' 4) + (solve' 7)
    where ss' = map getLast ss
          getLast s = splitOn " " $ (splitOn " | " s) !! 1
          solve' i = sum $ map (length . (filter (\x -> length x == i))) ss'

calculateMap :: [String] -> MState ()
calculateMap xs = do
    updateM 1 (ofLength 2 xs)
    updateM 7 (ofLength 3 xs)
    updateM 4 (ofLength 4 xs)
    updateM 8 (ofLength 7 xs)
    getLength6 (filter (\x -> length x == 6) xs)
    getLength5 (filter (\x -> length x == 5) xs)


getLength5 :: [String] -> MState ()
getLength5 xs = do
    one <- lookupM 1
    updateM 3 (headOrError $ filter (\x -> length (x `union` one) == length x) xs)
    six <- lookupM 6
    updateM 5 (headOrError $ filter (\x -> length (x `intersect` six) == length x) xs)
    five <- lookupM 5
    three <- lookupM 3
    updateM 2 (headOrError $ filter (\x -> x /= five && x /= three) xs)

getLength6 :: [String] -> MState ()
getLength6 xs = do
    seven <- lookupM 7
    four <- lookupM 4
    updateM 6 $ headOrError $ filter (\x -> length (x `union` seven) /= length x) xs
    six <- lookupM 6
    updateM 0 $ headOrError $ filter (\x -> x /= six && length (x `union` four) /= length x) xs
    zero <- lookupM 0
    updateM 9 (headOrError $ filter (\x -> x /= six && x /= zero) xs)


ofLength :: Int -> [String] -> String
ofLength l = headOrError . filter (\x -> length x == l)

headOrError :: [String] -> String
headOrError xs = if length xs == 1 then head xs else if length xs > 1 then error $ "Too long list " ++ show xs else error "Empty list"
