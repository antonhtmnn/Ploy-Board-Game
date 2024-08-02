module Ploy where -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, fromJust)
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift )



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Implementation Points     #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

gameFinished :: Board -> Bool
gameFinished board =
  not
  ((containsCell board (Piece Black 170) 0 0 || containsCell board (Piece Black 85) 0 0)
  && (containsCell board (Piece White 170) 0 0 || containsCell board (Piece White 85) 0 0)
  && not (null (getCellsOf board Black 0 0 []))
  && not (null (getCellsOf board White 0 0 [])))

-- True if board contains cell, else False
containsCell :: Board -> Cell -> Int -> Int -> Bool
containsCell board cell row col
  | row == 9 = False
  | col == 9 = containsCell board cell (row + 1) 0
  | (board!!row)!!col == cell = True
  | otherwise = containsCell board cell row (col + 1)

-- Returns a list of all cells (without Commander) from Player p
getCellsOf :: Board -> Player -> Int -> Int -> [Cell] -> [Cell]
getCellsOf board p row col cells
  | row == 9 = cells
  | col == 9 = getCellsOf board p (row + 1) 0 cells
  | player ((board!!row)!!col) == Just p
    && figure ((board!!row)!!col) /= Just 170 && figure ((board!!row)!!col) /= Just 85 =
    let newCells = cells ++ [(board!!row)!!col] in
      getCellsOf board p row (col + 1) newCells
  | otherwise = getCellsOf board p row (col + 1) cells

player :: Cell -> Maybe Player
player (Piece p _) = Just p
player _ = Nothing

figure :: Cell -> Maybe Int
figure (Piece _ n) = Just n
figure _ = Nothing



-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Implementation Points            ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

-- contains all possible instances of a shield
shield :: [Int]
shield = [1,2,4,8,16,32,64,128]

-- contains all possible instances of a probe
probe :: [Int]
probe = [17,34,68,136,
         130,5,10,20,40,80,160,65,
         129,3,6,12,24,48,96,192]

-- contains all possible instances of a lance
lance :: [Int]
lance = [131,7,14,28,56,112,224,193,
         146,37,74,148,41,82,164,73,
         69,138,21,42,84,168,81,162]

-- contains all possible instances of a commander
commander :: [Int]
commander = [170,85]

-- on index i this list contains a binary mask (to check the i-th position
-- of a binary number using the binary AND operator)
mask :: [Int]
mask = [1,    -- 00000001
        2,    -- 00000010
        4,    -- 00000100
        8,    -- 00001000
        16,   -- 00010000
        32,   -- 00100000
        64,   -- 01000000
        128]  -- 10000000

isValidMove :: Board -> Move -> Bool
isValidMove board (Move start target turn) =
  let (path, direction) = lineMaybe start target in
    (length path `elem` [1..4]
    && turn `elem` [0..7]
    && isValidMoveSelector board (path, direction) turn)

lineMaybe :: Pos -> Pos -> ([Pos], Int)
lineMaybe start target =
  let (path, direction) = lineAndDirection start target in
    if not . null $ drop 9 path then
      ([], err)
    else
      (path, direction)

isValidMoveSelector :: Board -> ([Pos], Int) -> Int -> Bool
isValidMoveSelector board (path, direction) turn
  | fig `elem` shield = isValidMoveShield board p fig (path, direction) turn
  | fig `elem` probe = isValidMoveProbe board p fig (path, direction) turn
  | fig `elem` lance = isValidMoveLance board p fig (path, direction) turn
  | fig `elem` commander = isValidMoveCommander board p fig (path, direction) turn
  | otherwise = False
  where fig = fromMaybe err (figure (getCellFromPos board (head path)))
        p = fromJust (player (getCellFromPos board (head path)))
            -- ^ on *ERROR* p will not be calculated because fig should also be -1)

isValidMoveShield :: Board -> Player -> Int -> ([Pos], Int) -> Int -> Bool
isValidMoveShield board p fig (path, direction) turn
  | length path == 1 = -- if no movement, at least the rotation should change the state of the board
    rotate fig turn /= fig
  | length path <= 2 =
    validMoveDirection fig direction && validMoveOnBoard board p path (head path)
  | otherwise = False

isValidMoveProbe :: Board -> Player -> Int -> ([Pos], Int) -> Int -> Bool
isValidMoveProbe board p fig (path, direction) turn
  | length path > 1 && turn > 0 = False -- probe can't move and turn at the same time
  | length path == 1 = -- if no movement, at least the rotation should change the state of the board
    rotate fig turn /= fig
  | length path <= 3 =
    validMoveDirection fig direction && validMoveOnBoard board p path (head path)
  | otherwise = False

isValidMoveLance :: Board -> Player -> Int -> ([Pos], Int) -> Int -> Bool
isValidMoveLance board p fig (path, direction) turn
  | length path > 1 && turn > 0 = False -- lance can't move and turn at the same time
  | length path == 1 = -- if no movement, at least the rotation should change the state of the board
    rotate fig turn /= fig
  | length path <= 4 =
    validMoveDirection fig direction && validMoveOnBoard board p path (head path)
  | otherwise = False

isValidMoveCommander :: Board -> Player -> Int -> ([Pos], Int) -> Int -> Bool
isValidMoveCommander board p fig (path, direction) turn
  | length path > 1 && turn > 0 = False -- commander can't move and turn at the same time
  | length path == 1 = -- if no movement, at least the rotation should change the state of the board
    rotate fig turn /= fig
  | length path <= 2 =
    validMoveDirection fig direction && validMoveOnBoard board p path (head path)
  | otherwise = False

-- True if the move can be performt on the current board (collision detection
-- of other figures on path and check if target position is free or an enemy)
-- NOTE: the list [Pos] should contain more than the start position (length xs > 1)
validMoveOnBoard :: Board -> Player -> [Pos] -> Pos -> Bool
validMoveOnBoard board p [x] _ = -- check target position
  let cell = getCellFromPos board x in
    cell == Empty || player cell /= Just p
validMoveOnBoard board p (x:xs) start =
  let cell = getCellFromPos board x in
    if x /= start then -- skip start position
      cell == Empty && validMoveOnBoard board p xs start
    else
      validMoveOnBoard board p xs start
validMoveOnBoard _ _ _ _ = False

-- True if the figure is allowed to move into the dirrection of the path
validMoveDirection :: Int -> Int -> Bool
validMoveDirection fig direction =
  fig .&. mask!!direction /= 0

-- returns the cell of a given position in board
getCellFromPos :: Board -> Pos -> Cell
getCellFromPos board pos =
  let (rowIdx, colIdx) = posToIdx pos in
    (board!!rowIdx)!!colIdx

-- converts Pos to board index
posToIdx :: Pos -> (Int, Int)
posToIdx (Pos col row) =
  let
    rowIdx = elemIndex row [9,8..1]
    colIdx = elemIndex col ['a'..'i']
  in
    (fromMaybe err rowIdx, fromMaybe err colIdx)



-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Implementation Points              ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

-- the arguments Pos and Cell must contain valid values
-- otherwise an empty list will be returned
possibleMoves :: Pos -> Cell -> [Move]
possibleMoves pos cell
  | fig `elem` shield =
    let paths = getAllPaths pos fig 'a' 1 2 [] in
      getMovesJustTurn pos fig ++ getMovesWithTurn paths
  | fig `elem` probe =
    let paths = getAllPaths pos fig 'a' 1 3 [] in
      getMovesJustTurn pos fig ++ getMoves paths
  | fig `elem` lance =
    let paths = getAllPaths pos fig 'a' 1 4 [] in
      getMovesJustTurn pos fig ++ getMoves paths
  | fig `elem` commander =
    let paths = getAllPaths pos fig 'a' 1 2 [] in
      getMovesJustTurn pos fig ++ getMoves paths
  | otherwise = []
  where fig = fromMaybe err (figure cell)

-- returns a list of moves that change in position (probe, lance, commander)
-- (all moves returned will change the state of the board)
getMoves :: [[Pos]] -> [Move]
getMoves [x] = [Move (head x) (last x) 0]
getMoves (x:xs) = Move (head x) (last x) 0 : getMoves xs
getMoves _ = []

-- returns a list of moves that change in position and turn (only allowed for shield)
-- (all moves returned will change the state of the board)
getMovesWithTurn :: [[Pos]] -> [Move]
getMovesWithTurn [path] = [Move (head path) (last path) turn | turn <- [0..7]]
getMovesWithTurn _ = []

-- returns a list of moves that only turn without a change in position
-- (all moves returned will change the state of the board)
getMovesJustTurn :: Pos -> Int -> [Move]
getMovesJustTurn pos fig = [Move pos pos turn | turn <- [0..7], rotate fig turn /= fig]

-- returns all paths (with 1 < lengthPath <= n) that are on a line with the start position
-- and only if the figure is allowed to move in that direction
getAllPaths :: Pos -> Int -> Char -> Int -> Int -> [[Pos]] -> [[Pos]]
getAllPaths _ _ 'j' _ n xs = filter (\x -> length x <= n) xs
getAllPaths startPos fig col 10 n xs = getAllPaths startPos fig (succ col) 1 n xs
getAllPaths startPos fig col row n xs =
  let (path, direction) = lineMaybe startPos (Pos col row) in
    if direction `elem` [0..7] && validMoveDirection fig direction then
      getAllPaths startPos fig col (row + 1) n (xs ++ [path])
    else
      getAllPaths startPos fig col (row + 1) n xs



-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Implementation Points                        ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

listMoves :: Board -> Player -> [Move]
listMoves board p =
  if not (gameFinished board) then
    listMovesHelper board p 'a' 1 []
  else
    []

listMovesHelper :: Board -> Player -> Char -> Int -> [Move] -> [Move]
listMovesHelper _ _ 'j' _ xs = xs
listMovesHelper board p col 10 xs = listMovesHelper board p (succ col) 1 xs
listMovesHelper board p col row xs
  | player cell == Just p =
    let moves = justValidMoves board (possibleMoves (Pos col row) cell) in
      listMovesHelper board p col (row + 1) (xs ++ moves)
  | otherwise = listMovesHelper board p col (row + 1) xs
  where cell = getCellFromPos board (Pos col row)

-- out of a list of possible moves only the valid moves are returned
justValidMoves :: Board -> [Move] -> [Move]
justValidMoves board xs = [move | move <- xs, isValidMove board move]
