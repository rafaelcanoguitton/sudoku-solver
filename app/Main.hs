module Main where


import Sudoku
import Control.Monad
import GenerateSudoku

--cabal run sudoku-solver.cabal


main :: IO ()
main = do
  putStrLn "\n\n **** SUDOKU SOLVER ****\n"
  putStrLn "\nGenerate Random Sudoku: \n"
  b <- return $ genBoard 30
  putStrLn $ showBoard b
  putStrLn "\nSalida a interface : \n"
  input <- return $ boardToString b
  putStrLn $ input
  putStrLn "\nSolve Sudoku: \n"
  case readGrid input of
    Nothing   -> putStrLn "Invalid input"
    Just grid -> case Sudoku.solve grid of
        Nothing    -> putStrLn "No solution found"
        Just grid' -> putStrLn $ showGrid grid' ++
         "\nSalida a interface : \n\n" ++ showGridConcat grid'

