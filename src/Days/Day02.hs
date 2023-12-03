module Days.Day02 (runDay,inputParser) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Text (unpack)
import Data.Attoparsec.Combinator (lookAhead)
import Control.Applicative ((<|>), Alternative (empty))
import Data.Tuple (swap)
import Data.Function (on)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy line endOfLine
    where line = string "Game " >> decimal >> char ':' >> sepBy parseSet (char ';')
          parseSet = sepBy parseCube (char ',')
          parseCube = (,) <$> (space >> decimal <* space) <*> parseColor

          parseColor :: Parser Color
          parseColor = choice $ map (fmap (read . unpack) . string)  ["blue","green","red"]

------------ TYPES ------------
data Color = Blue | Green | Red deriving (Show,Eq,Ord)

instance Read Color where
    readsPrec _ c =  
        let (s, r) = span (/= ' ') c
        in case s of
            "green" -> [(Green,r)]
            "blue" -> [(Blue,r)]
            "red" -> [(Red,r)]

type Cube = (Int,Color)
type GameSet = [Cube]

type Game = [GameSet]

type Input = [Game]

type OutputA = Int

type OutputB = [[(Color,Int)]]

------------ PART A ------------
cap = [(Green,13),(Red,12),(Blue,14)]

partA :: Input -> OutputA
partA x = sum $ map snd $ filter (\(x,i) -> isValidGame x) $ zip x [1..]
    where isValidGame :: Game -> Bool
          isValidGame = not . any (any (\(n,color) -> maybe False (n >) (lookup color cap) ))

------------ PART B ------------
--partB :: Input -> OutputB
partB = sum . map (product . minCubes)
    where minCubes = map (snd . maximumBy (compare `on` snd)) .
           groupBy ((==) `on` fst) . sortBy (compare `on` fst) . map swap . concat -- . groupBy (\x y -> fst x == fst y) . concat
