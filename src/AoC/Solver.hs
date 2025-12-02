module AoC.Solver where

-- This approach was based on an idea by https://github.com/mstksg.

data a :->: b = MkSol
    { sParse :: String -> Maybe a
    , sSolve :: a -> Maybe b
    , sPrint :: b -> String
    }

data SolverError = ParseError | SolveError deriving (Show)

runSolution :: (a :->: b) -> String -> Either SolverError String
runSolution (MkSol sParse sSolve sPrint) raw = do
    input <- maybe (Left ParseError) Right (sParse raw)
    solution <- maybe (Left SolveError) Right (sSolve input)
    pure $ sPrint solution
