
import DayZero
import Control.Monad.State
import Debug.Trace
import qualified Data.Map as Map

data Player = Player {getScore :: Score, getPos :: Position}
    deriving (Eq, Ord, Show)

type Score = Integer
type Position = Integer
type Dice = Integer
type Game = State (Dice, Integer) 

type Game2 = State (Map.Map (Player, Player, Bool) (Integer, Integer))


main :: IO ()
main = do
    (p1:p2:[]) <- manuallyParse "d21.txt" "\n" (parsePlayer)
    let (_,loser, rolls) = evalState (run p1 p2) (1, 0)
    print $ (getScore loser) * rolls
    let m = Map.insert (p1, p2) 0 (Map.empty)
    print $ (uncurry max) $ evalState (run2 p1 p2 True) Map.empty

(+!) :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
(+!) (x, y) (u, v) = (x+u, y+v)

(*!) :: (Integer, Integer) -> Integer -> (Integer, Integer)
(*!) (x, y) n = (x*n, y*n)

run2 :: Player -> Player -> Bool -> Game2 (Integer, Integer)
run2 p1 p2 playerOneTurn
    | getScore p1 >= 21 = return (1, 0)
    | getScore p2 >= 21 = return (0, 1)
    | otherwise = do
        maybeScore <- cached p1 p2 playerOneTurn
        case maybeScore of
            (Just n) -> return n
            Nothing -> do
                let nextRun i = if playerOneTurn then run2 (roll2 p1 i) p2 $ not playerOneTurn else run2 p1 (roll2 p2 i) $ not playerOneTurn
                ss <- mapM nextRun [3..9]
                let s n = ss !! n
                let tot = ((s 0) +! ((s 1)*!3) +! ((s 2) *! 6) +! ((s 3) *! 7) +! ((s 4) *! 6) +! ((s 5) *! 3) +! (s 6))
                cache p1 p2 playerOneTurn tot
                return tot
                
roll2 :: Player -> Integer -> Player
roll2 (Player score pos) d = 
    let newPos = (pos + d - 1) `mod` 10 + 1
    in (Player (score + newPos) newPos)

cache :: Player -> Player ->Bool -> (Integer, Integer) -> Game2 ()
cache p1 p2 b s = do
    m <- get
    put $ Map.insert (p1, p2, b) s m


cached :: Player -> Player -> Bool -> Game2 (Maybe (Integer, Integer))
cached p1 p2 b = do
    m <- get
    return $ m Map.!? (p1, p2, b)

parsePlayer :: String -> Player
parsePlayer s = Player 0 (read [last s])

run :: Player -> Player -> Game (Player, Player, Integer)
run p1 p2 = do
    p1' <- roll p1
    if (getScore p1') >= 1000 
        then do
            (_, rolls) <- get
            return (p1', p2, rolls)
        else do
            p2' <- roll p2
            if (getScore p2' ) >= 1000
                then do
                    (_, rolls) <- get
                    return (p2', p1', rolls)
                else run p1' p2'

roll :: Player -> Game Player
roll (Player score pos) = do
    (d, rolls) <- get
    let pos' = (pos + d + d+1 + d+2 - 1) `mod` 10 + 1
    let d' = (d + 3 - 1) `mod` 100 + 1
    put (d', rolls + 3)
    return (Player (score + pos') (pos'))
