import DayZero
import Data.List.Split
import Data.List
import qualified Data.Map as M

data Cave = Big String | Small String
    deriving (Ord, Eq)

instance Show Cave where
    show (Big s) = s
    show (Small s) = s

type Path = M.Map Cave [Cave]

cave :: String -> Cave
cave name@(c:cs)
    | c `elem` ['A'..'Z'] = Big name
    | otherwise = Small name



main :: IO ()
main = do
    input' <- manuallyParse "d12.txt" "\n" (\x -> let (a:b:[]) = splitOn "-" x in [(cave a, cave b), (cave b, cave a)])
    let input = concat input'
    let allCaves = uniq $ map fst input
    let caveMap = getCaveMap input allCaves
    print $ length $ solve caveMap
    print $ length $ uniq $ solve2 caveMap


solve :: Path -> [[Cave]]
solve p = solve' p (Small "start") []

solve2 :: Path -> [[Cave]]
solve2 p = solve2' p (Small "start") [(Small "start")]

big :: Cave -> Bool
big (Big _) = True
big _ = False

solve' :: Path -> Cave -> [Cave] -> [[Cave]] 
solve' p cave visited =
           let nextCaves = filter (\c -> not $ c `elem` visited) $ p M.! cave
               visited' = if big cave then visited else cave:visited
               solutions = concatMap (\next ->  solve' p next visited') nextCaves
            in if cave == (Small "end") then [[cave]] else map (cave :) solutions

solve2' :: Path -> Cave -> [Cave] -> [[Cave]] 
solve2' p cave visited =
           let nextCaves = filter (\c -> not $ c `elem` visited || c == (Small "end")) $ p M.! cave
               visited' = if big cave then visited else cave:visited
               solutions1 = concatMap (\next ->  solve' p next visited) nextCaves
               solutions2 = concatMap (\next ->  solve2' p next visited') nextCaves
            in if big cave then map (cave :) solutions2 else (map (cave :) solutions2) ++ (map (cave :) solutions1)


getCaveMap :: [(Cave, Cave)] -> [Cave] -> Path
getCaveMap = getCaveMap' M.empty
    where getCaveMap' path caves (x:xs) = getCaveMap' (M.insert x (map snd $ filter (\(a, b) -> a == x) caves) path) caves xs
          getCaveMap' p _ [] = p
