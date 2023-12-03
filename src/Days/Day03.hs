module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Control.Applicative ((<|>), Alternative (empty))
import qualified Program.RunDay as R (runDay, Day)
import Data.List.Extra (firstJust)
import Data.Attoparsec.Text
import Data.Void
import Data.Char (digitToInt)
import Data.Tuple.Extra (second)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
data EngineSymbol = Digit Int | Symbol Char | Empty deriving (Show,Eq)

inputParser :: Parser Input
inputParser = many' (manyTill' parseCharacter endOfLine)
    where parseCharacter = (Digit . digitToInt <$> digit) <|> (Empty <$ char '.') <|> (Symbol <$> anyChar)

------------ TYPES ------------
type Input = [[EngineSymbol]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
type EngineSchematic = [((Int,Int),EngineSymbol)]

toCoordinates :: [[EngineSymbol]] ->  EngineSchematic
toCoordinates x = concat $ zipWith (\a y -> zipWith (\b x -> ((y,x),b)) a inf) x inf
    where inf = [0..]

foldCoordinates ys = sum $ map (toNumber . map snd) $ filter ( any (\(c,n) -> not . null $ lookupSymbol c isSymbol ys)) $ snd $ foldl groupDigits ([],[]) ys

toNumber :: [Int] -> Int
toNumber = foldl (\acc x -> acc * 10 + x) 0 . reverse

lookupSymbol :: (Int, Int) -> (EngineSymbol -> Bool) -> [((Int, Int), EngineSymbol)] -> [(Int, Int)]
lookupSymbol (y,x) pred xs = mapMaybe (\n -> firstJust (\(c,s) -> if c == n && pred s then Just c else Nothing) xs) neighbours
    where neighbours = [(y-1,x),(y+1,x),(y,x-1),(y,x+1),(y-1,x-1),(y-1,x+1),(y+1,x+1),(y+1,x-1)]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe pred [] = []
mapMaybe pred (x:xs) = case pred x of 
    Just y -> y : mapMaybe pred xs
    Nothing -> mapMaybe pred xs

groupDigits ([],state) (_,Symbol _) = ([],state)
groupDigits ([],state) (_,Empty) = ([],state)
groupDigits (prev,state) (_,Symbol _) = ([],prev : state)
groupDigits (prev,state) (_,Empty) = ([],prev : state)
groupDigits (prev,state) (c,Digit n) = ((c,n) : prev,state)

isSymbol (Symbol _) = True
isSymbol _ = False
          
isDigit (Digit _) = True
isDigit _ = False

getDigit (Digit x) = x
getDigit _ = error "not digit"

partA :: Input -> OutputA
partA = foldCoordinates . toCoordinates

------------ PART B ------------
foldGearRatio :: [((Int, Int), EngineSymbol)] -> [Int]
foldGearRatio ys = foldl go [] $ filter ((==) (Symbol '*') . snd) ys
    where digitsMap = map (map (second Digit)) $ snd $ foldl groupDigits ([],[]) ys
          go acc (c,s) = if length matches == 2 then calcGearRatio matches : acc else acc
            where digitsCoordinates = lookupSymbol c isDigit $ concat digitsMap
                  matches = map (map (getDigit . snd)) $ filter (any ((`elem` digitsCoordinates) . fst )) digitsMap

calcGearRatio :: [[Int]] -> Int
calcGearRatio = product . map toNumber

partB :: Input -> OutputB
partB = sum . foldGearRatio . toCoordinates
