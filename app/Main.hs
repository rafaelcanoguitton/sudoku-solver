module Main where
import Sudoku
import Control.Monad
import GenerateSudoku
import System.Environment ( getArgs )
--cabal run sudoku-solver.cabal


main :: IO ()
main = do
  putStrLn "\n\n **** SUDOKU SOLVER ****\n"
  --Commandline arguments
  args <- getArgs
  --If no arguments are given, generate a sudoku
  if null args then do
    putStrLn "\nGenerate Random Sudoku: \n"
    let b = genBoard 30
    putStrLn $ showBoard b
    putStrLn "\nSalida a interface : \n"
    let input = boardToString b
    putStrLn input
  --If arguments are given, solve the sudoku
  else do
    putStrLn "\nSolve Sudoku: \n"
    let input = head args
    case readGrid input of
      Nothing   -> putStrLn "Invalid input"
      Just grid -> case Sudoku.solve grid of
          Nothing    -> putStrLn "No solution found"
          Just grid' -> putStrLn $ showGrid grid' ++
           "\nSalida a interface : \n\n" ++ showGridConcat grid'