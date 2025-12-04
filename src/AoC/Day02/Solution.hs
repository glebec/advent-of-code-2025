module AoC.Day02.Solution where

import AoC.Solver ((:->:) (..))

import Data.IntSet (IntSet, findMax, findMin, fromRange)
import Data.List (unfoldr)
import Data.Void (Void)
import GHC.Num (integerLogBase)
import Text.Megaparsec (Parsec, parseMaybe, sepEndBy)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

pRange :: Parser IntSet
pRange = curry fromRange <$> decimal <*> (char '-' *> decimal)

-- Get next number that is a decimal value repeated, e.g. 123100 -> 123123.
nextFakeId :: Int -> Int
nextFakeId n
    | length (show n) `mod` 2 == 1 =
        nextFakeId $ 10 ^ (integerLogBase 10 (toInteger n) + 1)
nextFakeId n =
    let (h1, h2) = splitAt (length (show n) `div` 2) (show n)
     in if read @Int h1 >= read @Int h2
            then read $ h1 <> h1
            else
                let h1' = show @Int (read @Int h1 + 1)
                 in read $ h1' <> h1'

fakeIdsInRange :: IntSet -> [Int]
fakeIdsInRange range = unfoldr go $ nextFakeId (findMin range)
  where
    go fakeId = if fakeId > upper then Nothing else Just (fakeId, nextFakeId $ fakeId + 1)
    upper = findMax range

day02a :: [IntSet] :->: Int
day02a =
    MkSol
        { sParse = parseMaybe $ sepEndBy pRange (char ',' <* space)
        , sSolve = Just . sum . concatMap fakeIdsInRange
        , sPrint = show
        }
