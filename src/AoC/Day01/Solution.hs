module AoC.Day01.Solution where

import AoC.Solver ((:->:) (..))
import Data.Foldable (foldl')

import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

import Control.Applicative ((<|>))
import Data.Functor (($>))

type Parser = Parsec Void String

data DialInput = L Int | R Int deriving (Show, Eq)

pDialInput :: Parser DialInput
pDialInput = ((char 'L' $> L) <|> (char 'R' $> R)) <*> decimal <* space

applyInput :: Int -> DialInput -> Int
applyInput start (L clicks) = (start - clicks) `mod` 100
applyInput start (R clicks) = (start + clicks) `mod` 100

-- Day 1A

day01a :: [DialInput] :->: Int
day01a =
    MkSol
        { sParse = parseMaybe $ many pDialInput
        , sSolve = Just . length . filter (== 0) . scanl applyInput 50
        , sPrint = show
        }

-- Day 1B

-- (current pos, num zero clicks) -> DialInput -> (ending pos, num zero clicks)
applyInputCountingLoops :: (Int, Int) -> DialInput -> (Int, Int)
applyInputCountingLoops (start, zeros) (L clicks) =
    let (negLoops, next) = (start - clicks) `divMod` 100
        z' = if start == 0 then abs negLoops - 1 else abs negLoops -- start at 0 means first loop didn't have a zero click
     in (next, zeros + z' + if next == 0 then 1 else 0)
applyInputCountingLoops (start, zeros) (R clicks) =
    let (loops, next) = (start + clicks) `divMod` 100
     in (next, zeros + loops)

day01b :: [DialInput] :->: Int
day01b =
    MkSol
        { sParse = parseMaybe $ many pDialInput
        , sSolve = Just . snd . foldl' applyInputCountingLoops (50, 0)
        , sPrint = show
        }
