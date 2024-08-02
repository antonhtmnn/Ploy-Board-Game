-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################

import Test.Hspec

import Board
    ( validateFEN,
      buildBoard,
      line,
      Board,
      Cell(Empty, Piece),
      Player(Black, White),
      Pos(Pos, col, row),
      lineVert,
      lineHori,
      lineDiag )

import Ploy
    ( gameFinished,
      isValidMove,
      possibleMoves,
      listMoves,
      Move(Move, start, target, turn),
      getCellsOf,
      figure,
      isValidMoveProbe,
      isValidMoveLance,
      validMoveOnBoard,
      posToIdx,
      getMoves,
      getMovesWithTurn )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testLineVert
    testLineHori
    testLineDiag
    testGameFinished
    testGetCellsOf
    testFigure
    testIsValidMove
    testIsValidMoveProbe
    testIsValidMoveLance
    testValidMoveOnBoard
    testPosToIdx
    testPossibleMoves
    testGetMoves
    testGetMovesWithTurn
    testListMoves
    testGivenImplementation

startFEN :: String
startFEN = ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,"

emptyFEN :: String
emptyFEN = ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,"

startBoard :: Board
startBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],
              [Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],
              [Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],
              [Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],
              [Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

startBoardModified :: Board
startBoardModified = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],
                      [Empty,Piece Black 69,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],
                      [Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Piece Black 170,Empty,Empty],
                      [Empty,Empty,Empty,Piece Black 1,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Empty,Piece Black 17,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Piece Black 1,Piece Black 1,Empty,Empty,Empty],
                      [Empty,Empty,Piece Black 3,Piece Black 130,Empty,Piece Black 130,Piece Black 129,Empty,Empty],
                      [Empty,Empty,Piece Black 146,Piece Black 131,Empty,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

emptyBoard :: Board
emptyBoard = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
              [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

-- does not contain white Commander (Piece White 170)
sampleBoard :: Board
sampleBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],
               [Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],
               [Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],
               [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
               [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
               [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
               [Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],
               [Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],
               [Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

-- does not contain black Commander (Piece Black 170)
sampleBoard02 :: Board
sampleBoard02 = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],
                [Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],
                [Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],
                [Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],
                [Empty,Piece Black 69,Piece Black 146,Piece Black 131,Empty,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

-- contains both commanders and one other figure per player
almostEmptyBoard :: Board
almostEmptyBoard = [[Piece White 170,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Piece Black 74,Empty,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Empty,Piece White 1,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                    [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 85,Empty]]

-- contains both commanders and one other figure (probe) per player
almostEmptyBoard02 :: Board
almostEmptyBoard02 = [[Piece White 170,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 170],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 5],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Piece White 17,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                      [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

-- contains a figure (Piece White 200) that does not exist
boardWithFigure200 :: Board
boardWithFigure200 = [[Piece White 170,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 170],
                        [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 5],
                        [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Piece White 200,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

testValidateFEN :: Spec
testValidateFEN = describe "Module Board: validateFEN ..." $ do
    it "fen has not 9 rows" $ do
        validateFEN ",,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)
    it "validate startFEN" $ do
        validateFEN startFEN `shouldBe` (True :: Bool)
    it "validate emptyFEN" $ do
        validateFEN emptyFEN `shouldBe` (True :: Bool)

testBuildBoard :: Spec
testBuildBoard = describe "Module Board: buildBoard ..." $ do
    it "build empty board" $ do
        buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]
    it "build startBoard out of startFEN" $ do
        buildBoard startFEN `shouldBe` (startBoard :: Board)
    it "build emptyBoard out of emptyFEN" $ do
        buildBoard emptyFEN `shouldBe` (emptyBoard :: Board)

testLine :: Spec
testLine = describe "Module Board: line ..." $ do
    it "start is target" $ do
        line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([Pos 'a' 1] :: [Pos])
    it "vertical up" $ do
        line (Pos 'b' 2) (Pos 'b' 7) `shouldBe` ([Pos 'b' 2, Pos 'b' 3, Pos 'b' 4, Pos 'b' 5, Pos 'b' 6, Pos 'b' 7] :: [Pos])
    it "vertical down" $ do
        line (Pos 'd' 9) (Pos 'd' 1) `shouldBe` ([Pos 'd' 9, Pos 'd' 8, Pos 'd' 7, Pos 'd' 6, Pos 'd' 5, Pos 'd' 4, Pos 'd' 3, Pos 'd' 2, Pos 'd' 1] :: [Pos])
    it "horizontal right" $ do
        line (Pos 'c' 2) (Pos 'g' 2) `shouldBe` ([Pos 'c' 2, Pos 'd' 2, Pos 'e' 2, Pos 'f' 2, Pos 'g' 2] :: [Pos])
    it "horizontal left" $ do
        line (Pos 'i' 6) (Pos 'a' 6) `shouldBe` ([Pos 'i' 6, Pos 'h' 6, Pos 'g' 6, Pos 'f' 6, Pos 'e' 6, Pos 'd' 6, Pos 'c' 6, Pos 'b' 6, Pos 'a' 6] :: [Pos])
    it "diagonal up right" $ do
        line (Pos 'e' 7) (Pos 'g' 9) `shouldBe` ([Pos 'e' 7, Pos 'f' 8, Pos 'g' 9] :: [Pos])
    it "diagonal down left" $ do
        line (Pos 'i' 9) (Pos 'a' 1) `shouldBe` ([Pos 'i' 9, Pos 'h' 8, Pos 'g' 7, Pos 'f' 6, Pos 'e' 5, Pos 'd' 4, Pos 'c' 3, Pos 'b' 2, Pos 'a' 1] :: [Pos])
    it "diagonal up left" $ do
        line (Pos 'i' 4) (Pos 'f' 7) `shouldBe` ([Pos 'i' 4, Pos 'h' 5, Pos 'g' 6, Pos 'f' 7] :: [Pos])
    it "diagonal down right" $ do
        line (Pos 'a' 9) (Pos 'i' 1) `shouldBe` ([Pos 'a' 9, Pos 'b' 8, Pos 'c' 7, Pos 'd' 6, Pos 'e' 5, Pos 'f' 4, Pos 'g' 3, Pos 'h' 2, Pos 'i' 1] :: [Pos])

testLineVert :: Spec
testLineVert = describe "Module Board: lineVert (help function for line) ..." $ do
    it "check lineVert with static position as input" $ do
        lineVert (Pos 'g' 7) (Pos 'g' 7) `shouldBe` (([], -1) :: ([Pos], Int))

testLineHori :: Spec
testLineHori = describe "Module Board: lineHori (help function for line) ..." $ do
    it "check lineHori with static position as input" $ do
        lineHori (Pos 'g' 7) (Pos 'g' 7) `shouldBe` (([], -1) :: ([Pos], Int))

testLineDiag :: Spec
testLineDiag = describe "Module Board: lineDiag (help function for line) ..." $ do
    it "check lineDiag with static position as input" $ do
        lineDiag (Pos 'g' 7) (Pos 'g' 7) `shouldBe` (([], -1) :: ([Pos], Int))

testGameFinished :: Spec
testGameFinished = describe "Module Ploy: gameFinished ..." $ do
    it "check if startBoard is finished" $ do
        gameFinished startBoard `shouldBe` (False :: Bool)
    it "check if sampleBoard (no white Commander) is finished" $ do
        gameFinished sampleBoard `shouldBe` (True :: Bool)
    it "check if sampleBoard02 (no black Commander) is finished" $ do
        gameFinished sampleBoard02 `shouldBe` (True :: Bool)
    it "check if almostEmptyBoard (both commanders and one other figure per player) is finished" $ do
        gameFinished almostEmptyBoard `shouldBe` (False :: Bool)

testGetCellsOf :: Spec
testGetCellsOf = describe "Module Ploy: getCellsOf (help function for gameFinished) ..." $ do
    it "get cells of startBoard (Black)" $ do
        getCellsOf startBoard Black 0 0 [] `shouldBe` ([Piece Black 1,Piece Black 1,Piece Black 1,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 131,Piece Black 146,Piece Black 69] :: [Cell])

testFigure :: Spec
testFigure = describe "Module Ploy: figure (help function for gameFinished) ..." $ do
    it "get figure of (Piece Black 170)" $ do
        figure (Piece Black 170) `shouldBe` (Just 170 :: Maybe Int)
    it "get figure of (Empty)" $ do
        figure Empty `shouldBe` (Nothing :: Maybe Int)

testIsValidMove :: Spec
testIsValidMove = describe "Module Ploy: isValidMove ..." $ do
    it "rotation by 1 is always possible" $ do
        isValidMove sampleBoard (Move (Pos 'c' 1) (Pos 'c' 1) 1) `shouldBe` (True :: Bool)
    it "try to move commander in startBoard" $ do
        isValidMove startBoard (Move (Pos 'e' 1) (Pos 'f' 2) 0) `shouldBe` (False :: Bool)
    it "try to turn commander in startBoard" $ do
        isValidMove startBoard (Move (Pos 'e' 1) (Pos 'e' 1) 1) `shouldBe` (True :: Bool)
    it "try to turn commander in startBoard (with no change in state)" $ do
        isValidMove startBoard (Move (Pos 'e' 1) (Pos 'e' 1) 4) `shouldBe` (False :: Bool)
    it "try to move and turn shield" $ do
        isValidMove almostEmptyBoard (Move (Pos 'd' 3) (Pos 'd' 4) 6) `shouldBe` (True :: Bool)
    it "try to move shield" $ do
        isValidMove almostEmptyBoard (Move (Pos 'd' 3) (Pos 'd' 4) 0) `shouldBe` (True :: Bool)
    it "try to move shield to far" $ do
        isValidMove startBoard (Move (Pos 'f' 3) (Pos 'f' 5) 0) `shouldBe` (False :: Bool)
    it "try to move and turn probe" $ do
        isValidMove almostEmptyBoard02 (Move (Pos 'b' 2) (Pos 'b' 1) 1) `shouldBe` (False :: Bool)
    it "try to move probe" $ do
        isValidMove almostEmptyBoard02 (Move (Pos 'b' 2) (Pos 'b' 1) 0) `shouldBe` (True :: Bool)
    it "try to move probe to far" $ do
        isValidMove almostEmptyBoard02 (Move (Pos 'b' 2) (Pos 'b' 7) 0) `shouldBe` (False :: Bool)
    it "try to turn probe (with no change in state)" $ do
        isValidMove almostEmptyBoard02 (Move (Pos 'b' 2) (Pos 'b' 2) 4) `shouldBe` (False :: Bool)
    it "try to move and turn lance" $ do
        isValidMove almostEmptyBoard (Move (Pos 'c' 7) (Pos 'f' 4) 1) `shouldBe` (False :: Bool)
    it "try to move lance" $ do
        isValidMove almostEmptyBoard (Move (Pos 'c' 7) (Pos 'f' 4) 0) `shouldBe` (True :: Bool)
    it "try to move lance to far" $ do
        isValidMove almostEmptyBoard (Move (Pos 'c' 7) (Pos 'g' 3) 0) `shouldBe` (False :: Bool)
    it "try to move and turn commander" $ do
        isValidMove almostEmptyBoard (Move (Pos 'a' 9) (Pos 'b' 8) 1) `shouldBe` (False :: Bool)
    it "try to move commander" $ do
        isValidMove almostEmptyBoard (Move (Pos 'a' 9) (Pos 'b' 8) 0) `shouldBe` (True :: Bool)
    it "try to move commander to far" $ do
        isValidMove almostEmptyBoard (Move (Pos 'h' 1) (Pos 'h' 3) 0) `shouldBe` (False :: Bool)
    it "try to move commander out of boundaries" $ do
        isValidMove almostEmptyBoard (Move (Pos 'h' 1) (Pos 'h' 42) 0) `shouldBe` (False :: Bool)
    it "try to turn non existing figure (Piece White 200)" $ do
        isValidMove boardWithFigure200 (Move (Pos 'b' 2) (Pos 'b' 2) 1) `shouldBe` (False :: Bool)
    it "try to move non existing figure (Piece White 200)" $ do
        isValidMove boardWithFigure200 (Move (Pos 'b' 2) (Pos 'b' 3) 0) `shouldBe` (False :: Bool)
    it "try to turn empty cell" $ do
        isValidMove startBoard (Move (Pos 'b' 4) (Pos 'b' 4) 1) `shouldBe` (False :: Bool)
    it "try to move empty cell" $ do
        isValidMove startBoard (Move (Pos 'b' 4) (Pos 'b' 5) 0) `shouldBe` (False :: Bool)
    it "try stuff" $ do
        isValidMove startBoardModified (Move (Pos 'd' 6) (Pos 'd' 7) 1) `shouldBe` (True :: Bool)
        isValidMove startBoardModified (Move (Pos 'g' 7) (Pos 'f' 8) 0) `shouldBe` (True :: Bool)
        isValidMove startBoardModified (Move (Pos 'b' 8) (Pos 'b' 9) 0) `shouldBe` (True :: Bool)

testIsValidMoveProbe :: Spec
testIsValidMoveProbe = describe "Module Ploy: isValidMoveProbe (help function for isValidMove) ..." $ do
    it "call isValidMoveProbe with bad arguments" $ do
        isValidMoveProbe startBoard Black 1 ([Pos 'a' 1, Pos 'a' 2, Pos 'a' 3, Pos 'a' 4], 1) 0 `shouldBe` (False :: Bool)

testIsValidMoveLance :: Spec
testIsValidMoveLance = describe "Module Ploy: isValidMoveLance (help function for isValidMove) ..." $ do
    it "call isValidMoveLance with bad arguments" $ do
        isValidMoveLance startBoard Black 1 ([Pos 'a' 1, Pos 'a' 2, Pos 'a' 3, Pos 'a' 4, Pos 'a' 5], 1) 0 `shouldBe` (False :: Bool)

testValidMoveOnBoard :: Spec
testValidMoveOnBoard = describe "Module Ploy: validMoveOnBoard (help function for isValidMove) ..." $ do
    it "call validMoveOnBoard with bad arguments" $ do
        validMoveOnBoard [] Black [] (Pos 'a' 1) `shouldBe` (False :: Bool)
    it "enemy figure on target position" $ do
        validMoveOnBoard startBoardModified Black [Pos 'f' 5, Pos 'f' 6, Pos 'f' 7] (Pos 'f' 5) `shouldBe` (True :: Bool)

testPosToIdx :: Spec
testPosToIdx = describe "Module Ploy: posToIdx (help function for isValidMove) ..." $ do
    it "convert position to index" $ do
        posToIdx (Pos 'c' 4) `shouldBe` ((5, 2):: (Int, Int))
        posToIdx (Pos 'h' 6) `shouldBe` ((3, 7):: (Int, Int))
    it "convert position that is out of boundaries to index" $ do
        posToIdx (Pos 'Z' 1337) `shouldBe` ((-1, -1):: (Int, Int))

testPossibleMoves :: Spec
testPossibleMoves = describe "Module Ploy: possibleMoves ..." $ do
        it "move shield one step" $ do
            possibleMoves (Pos 'd' 3) (Piece White 1) `shouldContain` ([Move (Pos 'd' 3) (Pos 'd' 4) 0] :: [Move])
        it "get possible moves of figure that does not exist" $ do
            possibleMoves (Pos 'd' 3) (Piece White 200) `shouldBe` ([] :: [Move])
        it "get possible moves from an empty cell" $ do
            possibleMoves (Pos 'd' 3) Empty `shouldBe` ([] :: [Move])

testGetMoves :: Spec
testGetMoves = describe "Module Ploy: getMoves (help function for possibleMoves) ..." $ do
    it "call getMoves with bad arguments" $ do
        getMoves [] `shouldBe` ([] :: [Move])

testGetMovesWithTurn :: Spec
testGetMovesWithTurn = describe "Module Ploy: getMovesWithTurn (help function for possibleMoves) ..." $ do
    it "call getMovesWithTurn with bad arguments" $ do
        getMovesWithTurn [] `shouldBe` ([] :: [Move])

testListMoves :: Spec
testListMoves = describe "Module Ploy: listMoves ..." $ do
    it "game finished" $ do
        listMoves sampleBoard Black `shouldBe` ([] :: [Move])
    it "list moves for player Black in almostEmptyBoard" $ do
        (Move (Pos 'h' 1) (Pos 'h' 1) 1 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'h' 1) (Pos 'h' 1) 3 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'h' 1) (Pos 'h' 1) 5 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'h' 1) (Pos 'h' 1) 7 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'h' 1) (Pos 'g' 1) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'h' 1) (Pos 'h' 2) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'h' 1) (Pos 'i' 1) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'c' 7) 1 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'c' 7) 2 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'c' 7) 3 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'c' 7) 4 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'c' 7) 5 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'c' 7) 6 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'c' 7) 7 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'b' 7) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'a' 7) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'd' 8) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'e' 9) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'd' 6) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'e' 5) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        (Move (Pos 'c' 7) (Pos 'f' 4) 0 `elem` listMoves almostEmptyBoard Black) `shouldBe` (True :: Bool)
        length (listMoves almostEmptyBoard Black) `shouldBe` 21
    it "list moves for player White in almostEmptyBoard" $ do
        (Move (Pos 'a' 9) (Pos 'a' 9) 1 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'a' 9) 3 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'a' 9) 5 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'a' 9) 7 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'b' 8) 0 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 3) 1 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 3) 2 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 3) 3 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 3) 4 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 3) 5 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 3) 6 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 3) 7 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 0 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 1 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 2 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 3 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 4 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 5 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 6 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        (Move (Pos 'd' 3) (Pos 'd' 4) 7 `elem` listMoves almostEmptyBoard White) `shouldBe` (True :: Bool)
        length (listMoves almostEmptyBoard White) `shouldBe` 20
    it "list moves for player White in almostEmptyBoard02" $ do
        (Move (Pos 'a' 9) (Pos 'a' 9) 1 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'a' 9) 3 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'a' 9) 5 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'a' 9) 7 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'a' 9) (Pos 'b' 8) 0 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 2) 1 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 2) 2 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 2) 3 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 2) 5 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 2) 6 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 2) 7 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 1) 0 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 3) 0 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        (Move (Pos 'b' 2) (Pos 'b' 4) 0 `elem` listMoves almostEmptyBoard02 White) `shouldBe` (True :: Bool)
        length (listMoves almostEmptyBoard02 White) `shouldBe` 14
    it "list moves for player Black in almostEmptyBoard02" $ do
        (Move (Pos 'i' 9) (Pos 'i' 9) 1 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 9) (Pos 'i' 9) 3 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 9) (Pos 'i' 9) 5 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 9) (Pos 'i' 9) 7 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 9) (Pos 'h' 8) 0 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 8) (Pos 'i' 8) 1 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 8) (Pos 'i' 8) 2 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 8) (Pos 'i' 8) 3 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 8) (Pos 'i' 8) 4 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 8) (Pos 'i' 8) 5 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 8) (Pos 'i' 8) 6 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        (Move (Pos 'i' 8) (Pos 'i' 8) 7 `elem` listMoves almostEmptyBoard02 Black) `shouldBe` (True :: Bool)
        length (listMoves almostEmptyBoard02 Black) `shouldBe` 12

testGivenImplementation :: Spec
testGivenImplementation = describe "Module Board and Ploy: test the given implementation ..." $ do
    it "test stuff" $ do
        show Black `shouldBe` ("Black" :: [Char])
        show (Piece Black 1) `shouldBe` ("Piece Black 1" :: [Char])
        show (Pos 'a' 1) `shouldBe` ("Pos {col = 'a', row = 1}" :: [Char])
        col (Pos 'a' 1) `shouldBe` ('a' :: Char)
        row (Pos 'a' 1) `shouldBe` (1 :: Int)
        start (Move (Pos 'a' 1) (Pos 'i' 9) 0) `shouldBe` (Pos 'a' 1 :: Pos)
        target (Move (Pos 'a' 1) (Pos 'i' 9) 0) `shouldBe` (Pos 'i' 9 :: Pos)
        turn (Move (Pos 'a' 1) (Pos 'i' 9) 0) `shouldBe` (0 :: Int)
        show (Move (Pos 'a' 1) (Pos 'i' 9) 0) `shouldBe` ("a1-i9-0" :: [Char])
