import DayZero
import Text.ParserCombinators.ReadP
import Data.Char (isAlphaNum, digitToInt)
import Numeric (readHex)
import Text.Printf (printf)
import Debug.Trace (trace)
import Numeric    (readInt)

data Packet = Lit Version Int | Operator Version Type [Packet]
    deriving (Show)

type Version = Int
type Type = Int

main :: IO ()
main = do
    input <- concatMap hexToBin <$> filter isAlphaNum <$> readFile "d16.txt"
    let packet = (fst . head) $ readP_to_S parser input
    print $ solve packet
    print $ solve2 packet

solve2 :: Packet -> Int
solve2 (Lit _ i) = i
solve2 (Operator _ typ ps) = 
    let solved = map solve2 ps in
    case typ of
        0 -> sum $ solved
        1 -> foldl1 (*) solved
        2 -> minimum solved
        3 -> maximum solved
        5 -> if (head solved) > (solved !! 1) then 1 else 0
        6 -> if (head solved) < (solved !! 1) then 1 else 0
        7 -> if (head solved) == (solved !! 1) then 1 else 0

solve :: Packet -> Int
solve (Lit v _) = v
solve (Operator v _ ps) = v + (sum $ map solve ps)

hexToBin :: Char -> String
hexToBin c = let (x,_):_  = readHex [c] in printf "%04b" (x::Int)

parser :: ReadP Packet
parser = do
    version <- toInt <$> takeBin 3
    typ <- toInt <$> takeBin 3
    if typ == 4 then do
        contained <- parserLit
        return $ Lit version $ toInt contained
    else do
        contained <- parserOp
        return $ Operator version typ contained

parserLit :: ReadP String
parserLit = do
    group <- takeBin 5
    let i = tail group
    if head group == '0' 
        then return i
        else parserLit >>= (\other -> return $ i ++ other)

parserMult :: String -> ReadP [Packet]
parserMult str = do
    let [(packet, rest)] = readP_to_S parser str
    if all (`elem` "0") rest then return [packet] else do
        other <- parserMult rest
        return $ packet : other

parserOp :: ReadP [Packet]
parserOp = do
    lengthType <- takeBin 1
    if lengthType == "0" then do
        length <- toInt <$> takeBin 15
        substr <- takeBin length
        parserMult substr
    else do
        l <- toInt <$> takeBin 11
        ahead <- look
        parseNumber l

parseNumber :: Int -> ReadP [Packet]
parseNumber 0 = return []
parseNumber i = do
    packet <- parser
    others <- parseNumber (i-1)
    return $ packet : others

toInt :: String -> Int
toInt = fst . head . readInt 2 (`elem` "01") digitToInt

takeBin :: Int -> ReadP String
takeBin i = count i bin

bin :: ReadP Char
bin = (choice . map char) "01"
