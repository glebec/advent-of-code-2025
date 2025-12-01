module AoC.Day01.Solution where

import AoC.Solver ((:->:) (..))

data DialInput = L Int | R Int deriving (Show, Eq)

parseDialInput :: String -> Maybe DialInput
parseDialInput ('L' : num) = Just (L $ read num)
parseDialInput ('R' : num) = Just (R $ read num)
parseDialInput _ = Nothing

applyInputs :: Int -> [DialInput] -> [Int]
applyInputs = scanl applyInput
  where
    applyInput :: Int -> DialInput -> Int
    applyInput current (L n) = (current - n) `mod` 100
    applyInput current (R n) = (current + n) `mod` 100

solveDialInput :: [DialInput] -> Int
solveDialInput = length . filter (== 0) . applyInputs 50

day01a :: [DialInput] :->: Int
day01a =
    MkSol
        { sParse = traverse parseDialInput . lines
        , sSolve = Just . solveDialInput
        , sPrint = show
        }
