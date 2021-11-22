
module GenerateSudoku where

--Generate and solve Sudoku

--By sudoku board we mean 9 lists of 9 Int- lists
--
-- example: board = [[5,3,0,0,7,0,0,0,0],
--                  [6,0,0,1,9,5,0,0,0],
--                  [0,9,8,0,0,0,0,6,0],
--                  [8,0,0,0,6,0,0,0,3],
--                  [4,0,0,8,0,3,0,0,1],
--                  [7,0,0,0,2,0,0,0,6],
--                  [0,6,0,0,0,0,2,8,0],
--                  [0,0,0,4,1,9,0,0,5],
--                  [0,0,0,0,8,0,0,7,9]]
--
--
-- Generating a solving board:
--   > genBoard (difficulty))
--      The difficulty is how many empty elements to have on the board: from 0 (generate a solved board)
--      up to 69 (generating a board with only 11 elements). A higher number means a harder board.
--      Invalid values ​​will be rounded to the nearest valid one.
--
-- Solve a board
--  > solve [board]
--    The board must be set as above.
--   If it is not valid for some reason, it will return [[]], if it is valid it will solve it


import Data.List
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Data.Char
import Data.List.Split 

type Coord = (Int, Int) --(x,y)
type Board = [Int]

test_board :: [Board]
test_board = [[5,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

emptyBoard :: Board
emptyBoard = [0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0]

a_Board :: Board
a_Board = [0,0,0,0,0,0,0,0,0,
          1,0,0,0,0,0,0,0,0,
          2,0,0,0,0,0,0,0,0,
          3,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,
          4,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0]


--AUXILIARY FUNCTIONS

--show a Board 
showBoard::[Board] -> String
showBoard rows =  join "\n" [join " " row | row <- rows]

--Concatenate a list with separator
join :: Show a => String -> [a] -> String
join _   []     = ""
join _   (x:[]) = show x
join sep (x:xs) = show x ++ sep ++ join sep xs

--Make a list from a list of lists 
flatten :: [Board] -> Board
flatten b = concat b 

--Returns a list to a list of lists
unflatten :: Board -> [Board]
unflatten [] = []
unflatten b = [(take 9 b)] ++ unflatten (drop 9 b)

--Returns the coordinates of an element of a index
getElementAtNum :: Int -> Coord
getElementAtNum i = ((i - 9 * (quot i 9)), (quot i 9))

--Returns the index of the coordinate 
getElementAtCoord :: Coord -> Int
getElementAtCoord (x, y) = x + y * 9

--Takes the column of the i-th element
columnAt :: Board -> Int -> [Int]
columnAt b i = helperColumnAt (getElementAtNum i) b
  where helperColumnAt (x, _) b = map (\y -> b !! getElementAtCoord (x, y)) [0..8]

--Takes the order of the i-th element
rowAt :: Board -> Int -> [Int]
rowAt b i = helperRowAt (getElementAtNum i) b
  where helperRowAt (_, y) b = map (\x -> b !! getElementAtCoord (x, y)) [0..8]

--Takes the i-th quadrant (3x3) from top to bottom and from left to right
quadAt :: Board -> Int -> [Int]
quadAt b i = helperQuadAt (getElementAtNum i) b
  where helperQuadAt (x, y) b = [b !! getElementAtCoord (xx + (3 * (quot x 3)), yy + (3 * (quot y 3))) | xx <- [0..2], yy <- [0..2]]

--Removes item (s) from a list
removeItemFromList :: Int -> [Int] -> [Int]
removeItemFromList _ [] = []
removeItemFromList x (l:ls) = if (x == l) then (removeItemFromList x ls) else [l] ++ (removeItemFromList x ls)

--Removes element encounters from the first sheet to the second
removeListFromList :: [Int] -> [Int] -> [Int]
romoveListFromList _ [] = []
removeListFromList [] l = l
removeListFromList (x:xs) l = removeListFromList xs (removeItemFromList x l)

--Finds if an item is present in a list
search :: [Int] -> Int -> Bool
search [] _       = False
search (x:xs) elm = if x == elm then True else search xs elm

--Checks if list items are unique
uniqueElems :: [Int] -> Bool
uniqueElems [] = True
uniqueElems (x:xs) = if (search xs x) then False else uniqueElems xs

--Checks the rules for a board
checkRules :: Board -> Bool
checkRules b = helperCheckRules b 0
  where helperCheckRules b i | i == 9  = True
                             | ((uniqueElems (removeItemFromList 0 (rowAt b (9 * i)))) &&
                             (uniqueElems (removeItemFromList 0 (columnAt b i))) &&
                             (uniqueElems (removeItemFromList 0 (quadAt b (3 * i))))) = helperCheckRules b (i+1)
                             | otherwise = False

--Checks if each element has a possible solution
validate :: [Board] -> Bool
validate b = helperValidate (flatten b) 0
  where helperValidate b i  | i == 81                    = True
                            | possibleElements b i == [] = False
                            | otherwise                  = helperValidate b (i + 1)

--Check the validity of a board
checkValidity :: [Board] -> Bool
checkValidity b = (checkRules (flatten b)) && (validate b)

--END OF AUXILIARY FUNCTIONS



--SOLUTION FUNCTIONS

--Returns the possible elements for a cell, if it is already filled it returns its value
possibleElements :: Board -> Int -> [Int]
possibleElements b i
    | i > length b  = []
    | (b !! i) == 0 = removeListFromList (columnAt b i ++ rowAt b i ++ quadAt b i) [1..9]
    | otherwise     = [b !! i]


--Changes the value of item i in a list
setElementAt :: Int -> [Int] -> Int -> [Int]
setElementAt n xs newElement = take n xs ++ [newElement] ++ drop (n + 1) xs

--Returns the next empty item
nextEmpty :: Board -> Int -> Int
nextEmpty _ 80 = 80
nextEmpty b i
    | b !! i == 0 = i
    | otherwise   = nextEmpty b (i + 1)

--The original board -> How far we have come -> Somewhat filled board -> Correct board
genSolution :: Board -> Int -> [Int] -> Board
genSolution _ _ [] = []
genSolution b 80 (x:xs)
    | xs == []  = setElementAt 80 b x
    | otherwise = []
genSolution b i (x:xs)
    | genNext == [] = genSolution b i xs
    | otherwise        = genNext
      where genNext = (genSolution newBord nextE (possibleElements newBord nextE))
              where newBord = setElementAt i b x
                    nextE = nextEmpty b (i + 1)

--END OF SOLVING FUNCTIONS




--Checks if sudoku is valid and if so solves it
solve :: [Board] -> [Board]
solve b = if (checkRules flatB) && (validate b) then unflatten (genSolution flatB 0 (possibleElements flatB 0)) else [[]]
  where flatB = flatten b


--GENERATION FUNCTIONS

--Take randomly a number in the interval [i, j]
getRandom :: Int -> Int -> Int
getRandom i j = unsafePerformIO (getStdRandom (randomR (i, j)))

--Returns a random list item
getRandomElem :: [Int] -> Int
getRandomElem [] = 0
getRandomElem l  = l !! (getRandom 0 ((length l) - 1))

--Generates a valid board with elements to the diagonal
genRandomBoard :: Board -> Board
genRandomBoard b = helperGenRandB b 0
  where helperGenRandB b i | i > 80     = b
                           | otherwise  =  helperGenRandB (setElementAt i b (getRandomElem (possibleElements b i))) (i + 10)

--Removes random non-zero items from a list
remRandElms :: Board -> Int -> Board
remRandElms b i = if (i < 69) then helperRandElms b i [0..80] else helperRandElms b 69 [0..80]
  where helperRandElms b i l
            | i == 0      = b
            | otherwise   = helperRandElms (setElementAt getRand b 0) (i - 1) (removeItemFromList getRand l)
              where getRand = getRandomElem l

--Generates a random Sudoku board
genBoard :: Int -> [Board]
genBoard hardness = unflatten (remRandElms (flatten (solve (unflatten (genRandomBoard emptyBoard)))) hardness)

--END OF GENERATION FUNCTIONS




--FUNCTIONS TO EXCHANGE WITH INTERFACE

--board to string 
boardToString :: [Board] -> String
boardToString b = concat [convert c | c <- concat b]
  where convert x = if x/= 0 then show x else "."

-- string to board
-- stringToBoard :: String -> Maybe [Board]
-- boardToString b
--   | length b /= 81 = Nothing
--   | otherwise  = 
--   concat [convert c | c <- concat b]
--   where convert x = if x/= 0 then show x else "."

-- readGrid :: String -> Maybe Board
-- readGrid s
--   | length s == 81 = traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
--   | otherwise      = Nothing
--   where
--     readCell '.' = 0
--     readCell c
--       | Data.Char.isDigit c && c > '0' =  Data.Char.digitToInt $ c
--       | otherwise = Nothing



--END OF FUNCTIONS TO EXCHANGE WITH INTERFACE




outputRandomBoard :: Int -> String
outputRandomBoard hardness = concat [convert c | c <- board]
    where convert x = if x/= 0 then show x else "."
          board  = concat $ genBoard hardness



--MAIN

mainGenerator = do
    putStrLn "\nGenerate Random Sudoku: \n"
    input <- return $ genBoard 30
    putStrLn $ showBoard input
    putStrLn "\nSalida a interface : \n"
    putStrLn $ boardToString input
    putStrLn "\nSolve Sudoku: \n"
    solved <- return $ solve input 
    putStrLn $ showBoard solved


  