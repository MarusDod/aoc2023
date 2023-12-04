module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sepBy line endOfLine
    where space = many' (char ' ')
          numberParser = space *> decimal <* space
          line = (,) <$> (string "Card" >> space >> decimal >> char ':' >> many' numberParser) <*> (string "|" >> many' numberParser)

countCommon _ [] = 0
countCommon [] _ = 0
countCommon (x:xs) ys = let a = if x `elem` ys then 1 else 0
        in a + countCommon xs ys

------------ TYPES ------------
type Input = [([Int],[Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map (uncurry calcPoints)
    where calcPoints xs = (\x -> if x == 0 then 0 else 2 ^ (x-1)) . countCommon xs


------------ PART B ------------
partB :: Input -> OutputB
partB xs = sum $ fst $ foldl traverseScratchCards ([],repeat 1) cards
    where cards = map (uncurry countCommon) xs
          traverseScratchCards (accum,times : next) x = (times: accum,zipWith (+) next $ replicate x times ++ repeat 0)