module Days.Day01 (runDay,lookAhead,numberParser,notFollowedBy) where

import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Text (Text, pack, unpack)
import Data.Functor (($>), void)
import GHC.Exception (throw)
import Control.Applicative ((<|>), Alternative (empty))
import Data.Attoparsec.Text as T
    ( choice,
      many',
      isEndOfLine,
      endOfLine,
      parseOnly,
      string,
      takeWhile1,
      anyChar,
      take,
      Parser )
import Data.Attoparsec.ByteString.Char8 (isDigit)
import Data.Attoparsec.Combinator
import Control.Monad (join)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
notFollowedBy p = join (lookAhead p $> empty <|> pure (pure ()) )

skipOneChar p = lookAhead p <* void anyChar 
    

numberParser :: Parser Int
numberParser = 
    let nums = zip ["one","two","three","four","five","six","seven","eight","nine"] [1..]
        p = choice $ map (\(x,y) -> string x $> y ) nums ++ 
            map (fmap (read . unpack) . string . pack . show) [1..9]
    in do
        many' (notFollowedBy p >> T.take 1) >> skipOneChar p <* many' (notFollowedBy p >> T.take 1)

inputParser :: Parser Input
inputParser = many' $ takeWhile1 (not . isEndOfLine) <* endOfLine

------------ TYPES ------------
type Input = [Text]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA =  sum . map (\x -> let digits = filter isDigit $ unpack x in read [head digits,last digits])

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map getNumbers
    where getNumbers x  = case parseOnly (manyTill numberParser endOfInput) x of
            Left err -> error err
            Right res -> toNumber $ edges res
            where toNumber (x,y) = x * 10 + y
                  edges x = (head x, last x)
