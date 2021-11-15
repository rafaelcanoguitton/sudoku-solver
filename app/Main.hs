module Main where

-- import Data.Semigroup (Arg (Arg))
-- import System.Environment

-- solver :: [String] -> [String]
-- solver (x : xs) = ["Solution1", "Solution2"] --Corresponding to their algorithms

-- main :: IO ()
-- main = do
--   print "Welcome to Sudoku Solver!"
--   args <- getArgs
--   if null args
--     then do
--       error "Please enter a sudoku puzzle and an algorithm."
--     else
--       if length args == 1
--         then
--           if head args == "--help"
--             then do
--               putStrLn "Usage: sudoku-solver <puzzle> <algorithm1> <algorithm2> ..."
--               putStrLn "Algorithms: Basic, ..."
--             else do
--               error "Please enter a sudoku puzzle and an algorithm."
--         else do
--           let solutions = solver args
--           print solutions


import Sudoku
import Control.Monad


--Read input txt : cat input.txt | cabal run sudoku-solver.cabal


main :: IO ()
main = mainCheck

