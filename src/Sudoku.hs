module Sudoku where

import Data.Char
import Data.List
-- In cabal use the command: cabal install --lib split
import Data.List.Split 
import Data.Maybe
import Data.Function
import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad



data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)
type Row  = [Cell]
type Grid = [Row]



-- INPUT/OUTPUT FUNCTIONS

-- function to read
readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 = traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just $ Possible [1..9]
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Fixed . Data.Char.digitToInt $ c
      | otherwise = Nothing



-- funtions to read more readable
showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x) = show x
    showCell _ = "."


-- funtion to read the ouput in one line "
showGridConcat :: Grid -> String
showGridConcat = concat . map (concat . map showCell)
  where
    showCell (Fixed x) = show x
    showCell _ = "."
    

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = unlines . map (unwords . map showCell)
  where
    showCell (Fixed x)     = show x ++ "          "
    showCell (Possible xs) =
      (++ "]")
      . Data.List.foldl' (\acc x -> acc ++ if x `elem` xs then show x else " ") "["
      $ [1..9]





-- FUNCTIONS TO PRUNING THE GRID

-- function to pruning the cells from a row
pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCell (Possible xs) = case xs Data.List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    pruneCell x = Just x

-- function to transform a sub-grids into rows
subGridsToRows :: Grid -> Grid
subGridsToRows =
  concatMap (\rows -> let [r1, r2, r3] = map (Data.List.Split.chunksOf 3) rows
                      in zipWith3 (\a b c -> a ++ b ++ c) r1 r2 r3)
  . Data.List.Split.chunksOf 3

-- funtion to pruning rows, columns and sub-grids
pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid =
  traverse pruneCells grid
  >>= fmap Data.List.transpose . traverse pruneCells . Data.List.transpose
  >>= fmap subGridsToRows . traverse pruneCells . subGridsToRows

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'
  where
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'




-- SOLVER FUNCTIONS

-- function to make two grid options
nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let (i, first@(Fixed _), rest) =
        fixCell
        . Data.List.minimumBy (compare `Data.Function.on` (possibilityCount . snd))
        . filter (isPossible . snd)
        . zip [0..]
        . concat
        $ grid
  in (replace2D i first grid, replace2D i rest grid)
  where
    isPossible (Possible _) = True
    isPossible _            = False

    possibilityCount (Possible xs) = length xs
    possibilityCount (Fixed _)     = 1

    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    fixCell _                    = error "Impossible case"

    replace2D :: Int -> a -> [[a]] -> [[a]]
    replace2D i v =
      let (x, y) = (i `quot` 9, i `mod` 9) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

-- functions to VERIFY GRID

isGridEmpty:: Grid -> Bool
isGridEmpty grid = if length g  == length possible_g then True else False
    where g = concat grid
          possible_g =  [ () | Possible _ <- concat grid ]


isGridFilled :: Grid -> Bool
isGridFilled grid = null [ () | Possible _ <- concat grid ]


isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid
  || any isInvalidRow (Data.List.transpose grid)
  || any isInvalidRow (subGridsToRows grid)
  where
    isInvalidRow row =
      let fixeds         = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)

    hasDups l = hasDups' l []

    hasDups' [] _ = False
    hasDups' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = hasDups' ys (y:xs)


-- SOLVE function
solve :: Grid -> Maybe Grid
solve grid = pruneGrid grid >>= solve'
  where
    solve' g
      | isGridInvalid g = Nothing
      | isGridFilled g  = Just g
      | otherwise       =
          let (grid1, grid2) = nextGrids g
          in solve grid1 <|> solve grid2



--CHECK 
data Status = Valid  |  Invalid  | Solved  |  Empty  

check :: Grid -> Status
check g 
    | isGridEmpty g = Empty 
    | solve g /= Nothing  && isGridFilled g = Solved
    | solve g == Nothing = Invalid
    | solve g /= Nothing = Valid


mainCheck :: IO ()
mainCheck = do
  inputs <- lines <$> getContents
  Control.Monad.forM_ inputs $ \input ->
    case readGrid input of
      Nothing   -> putStrLn "Invalid input"
      Just grid -> case check grid of
        Valid   -> putStrLn $ "\n" ++ showGrid grid ++ "\nThis puzzle has a solution"
        Invalid -> putStrLn $ "\n" ++ showGrid grid ++ "This puzzle has no solution"
        Solved  -> putStrLn $ "\n" ++ showGrid grid ++ " Congratulations, this is the correct solution"
        Empty   -> putStrLn $ "\n" ++ showGrid grid ++ "The grid is empty. Please enter some values "



mainSolve :: IO ()
mainSolve = do
  inputs <- lines <$> getContents
  Control.Monad.forM_ inputs $ \input ->
    case readGrid input of
      Nothing   -> putStrLn "Invalid input"
      Just grid -> case solve grid of
        Nothing    -> putStrLn "No solution found"
        Just grid' -> putStrLn $ showGrid grid'