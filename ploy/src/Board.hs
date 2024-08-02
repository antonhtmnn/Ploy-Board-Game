module Board where -- do NOT CHANGE export of module

-- IMPORTS HERE
import Data.List.Split (splitOn)
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Chars



-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = c1 == c2 && r1 == r2

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Implementation Points              ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

validateFEN :: String -> Bool
validateFEN xs =
  let rowsFEN = splitOn "/" xs in
    (length rowsFEN == 9 && validateFENHelper rowsFEN)

validateFENHelper :: [String] -> Bool
validateFENHelper [] = True
validateFENHelper (x:xs) =
  let singleRow = splitOn "," x in
    (length singleRow == 9 && validateFENCheckRow singleRow && validateFENHelper xs)

validateFENCheckRow :: [String] -> Bool
validateFENCheckRow = foldr ((&&) . validateFENCheckElem) True

validateFENCheckElem :: String -> Bool
validateFENCheckElem [] = True
validateFENCheckElem (x:xs) =
  not (null xs || length xs > 3)
  && (isInt xs && (x == 'b' || x == 'w') && (read xs :: Integer) `elem` [1..255])

isInt :: String -> Bool
isInt = foldr (\ x -> (&&) (x `elem` ['0' .. '9'])) True



-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

buildBoard :: String -> Board
buildBoard xs =
  let cells = replaceSlash xs in
    buildBoardHelper [] [] (splitOn "," cells) 0 0

replaceSlash :: String -> String
replaceSlash = map repl
  where repl '/' = ','
        repl c   = c

buildBoardHelper :: Board -> [Cell] -> [String] -> Int -> Int -> Board
buildBoardHelper board boardRow xs row col
  | row == 9 = board
  | col == 9 =
    buildBoardHelper (board ++ [boardRow]) [] xs (row + 1) 0
  | otherwise =
    let newBoardRow = boardRow ++ [buildBoardFillCell (head xs)] in
      buildBoardHelper board newBoardRow (tail xs) row (col + 1)

buildBoardFillCell :: String -> Cell
buildBoardFillCell [] = Empty
buildBoardFillCell (x:xs) =
  if x == 'b' then
    Piece Black (read xs :: Int)
  else
    Piece White (read xs :: Int)



-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Implementation Points    ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

err :: Int
err = -1

line :: Pos -> Pos -> [Pos]
line (Pos sCol sRow) (Pos eCol eRow)
  | sCol == eCol && sRow == eRow = [Pos sCol eRow]
  | sCol == eCol = fst (lineVert (Pos sCol sRow) (Pos eCol eRow)) -- case 1: vertical
  | sRow == eRow = fst (lineHori (Pos sCol sRow) (Pos eCol eRow)) -- case 2: horizintal
  | otherwise = fst (lineDiag (Pos sCol sRow) (Pos eCol eRow)) -- case 3: diagonal

lineVert :: Pos -> Pos -> ([Pos], Int)
lineVert (Pos sCol sRow) (Pos eCol eRow)
  | sRow < eRow = (lineVertUp (Pos sCol sRow) (Pos eCol eRow), 0) -- case 1.0: vertical up
  | sRow > eRow = (lineVertDown (Pos sCol sRow) (Pos eCol eRow), 4) -- case 1.1: vertical down
  | otherwise = ([], err)

lineVertUp :: Pos -> Pos -> [Pos]
lineVertUp (Pos sCol sRow) (Pos eCol eRow) =
  if sRow == eRow then
    [Pos eCol eRow]
  else
    Pos sCol sRow : lineVertUp (Pos sCol (sRow + 1)) (Pos eCol eRow)

lineVertDown :: Pos -> Pos -> [Pos]
lineVertDown (Pos sCol sRow) (Pos eCol eRow) =
  reverse (lineVertUp (Pos eCol eRow) (Pos sCol sRow))

lineHori :: Pos -> Pos -> ([Pos], Int)
lineHori (Pos sCol sRow) (Pos eCol eRow)
  | sCol < eCol = (lineHoriRight (Pos sCol sRow) (Pos eCol eRow), 2) -- case 2.0: horizontal right
  | sCol > eCol = (lineHoriLeft (Pos sCol sRow) (Pos eCol eRow), 6) -- case 2.1: horizontal left
  | otherwise = ([], err)

lineHoriRight :: Pos -> Pos -> [Pos]
lineHoriRight (Pos sCol sRow) (Pos eCol eRow) =
  if sCol == eCol then
    [Pos eCol eRow]
  else
    Pos sCol sRow : lineHoriRight (Pos (succ sCol) sRow) (Pos eCol eRow)

lineHoriLeft :: Pos -> Pos -> [Pos]
lineHoriLeft (Pos sCol sRow) (Pos eCol eRow) =
  reverse (lineHoriRight (Pos eCol eRow) (Pos sCol sRow))

lineDiag :: Pos -> Pos -> ([Pos], Int)
lineDiag (Pos sCol sRow) (Pos eCol eRow)
  | sRow < eRow && sCol < eCol =
    (lineDiagUR (Pos sCol sRow) (Pos eCol eRow) 0, 1) -- case 3.0: diagonal up-right
  | sRow > eRow && sCol > eCol =
    (reverse (lineDiagUR (Pos eCol eRow) (Pos sCol sRow) 0), 5) -- case 3.1: diagonal down-left
  | sRow < eRow && sCol > eCol =
    (lineDiagUL (Pos sCol sRow) (Pos eCol eRow) 0, 7) -- case 3.2: diagonal up-left
  | sRow > eRow && sCol < eCol =
    (reverse (lineDiagUL (Pos eCol eRow) (Pos sCol sRow) 0), 3) -- case 3.3: diagonal down-right
  | otherwise = ([], err)

-- if we get invalid Pos arguments (start - end position not on a line) we will be stuck in a loop
-- the first pattern will avoid such loops - so that in a worst case at most 10 elements will be returned
lineDiagUR :: Pos -> Pos -> Int -> [Pos]
lineDiagUR _ _ 10 = []
lineDiagUR (Pos sCol sRow) (Pos eCol eRow) count =
  if sCol == eCol && sRow == eRow then
    [Pos eCol eRow]
  else
    Pos sCol sRow : lineDiagUR (Pos (succ sCol) (sRow + 1)) (Pos eCol eRow) (count + 1)

-- if we get invalid Pos arguments (start - end position not on a line) we will be stuck in a loop
-- the first pattern will avoid such loops - so that in a worst case at most 10 elements will be returned
lineDiagUL :: Pos -> Pos -> Int -> [Pos]
lineDiagUL _ _ 10 = []
lineDiagUL (Pos sCol sRow) (Pos eCol eRow) count =
  if sCol == eCol && sRow == eRow then
    [Pos eCol eRow]
  else
    Pos sCol sRow : lineDiagUL (Pos (pred sCol) (sRow + 1)) (Pos eCol eRow) (count + 1)

lineAndDirection :: Pos -> Pos -> ([Pos], Int)
lineAndDirection (Pos sCol sRow) (Pos eCol eRow)
  | sCol == eCol && sRow == eRow = ([Pos sCol eRow], 42) -- no direction
  | sCol == eCol = lineVert (Pos sCol sRow) (Pos eCol eRow) -- case 1: vertical
  | sRow == eRow = lineHori (Pos sCol sRow) (Pos eCol eRow) -- case 2: horizintal
  | otherwise = lineDiag (Pos sCol sRow) (Pos eCol eRow) -- case 3: diagonal
