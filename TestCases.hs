module TestCases where

import Mancala
import GameIO
import Solve

-- Normal starting game
game1 :: Game
game1 = (One, ([4,4,4,4,4,4,0], [0,4,4,4,4,4,4]))

-- Player 1 has some empty wells
game2 :: Game
game2 = (One, ([4,0,2,0,3,0,0], [0,4,4,4,4,4,4]))

-- Player 2 has some empty wells
game3 :: Game
game3 = (Two, ([4,4,4,4,4,4,0], [0,0,2,0,3,0,1]))

-- Player 1 has won
game4 :: Game
game4 = (One, ([0,0,0,0,0,0,30], [5,0,0,0,0,0,0]))

-- Player 2 has won
game5 :: Game
game5 = (Two, ([0,0,0,0,0,0,10], [25,0,0,0,0,0,0]))

-- Draw
game6 :: Game
game6 = (One, ([0,0,0,0,0,0,24], [24,0,0,0,0,0,0]))

-- Player 1 captures marbles
game7 :: Game
game7 = (One, ([1,0,0,0,2,0,0], [0,4,4,4,4,4,4]))

-- Player 1 gets an extra turn
game8 :: Game
game8 = (One, ([4,4,4,4,4,1,0], [0,4,4,4,4,4,4]))

-- Player 2 captures marbles
game9 :: Game
game9 = (Two, ([4,4,4,4,4,4,0], [0,0,0,0,0,1,1]))

-- p2 gets another turn
game10 :: Game
game10 = (Two, ([4,4,4,4,4,4,0], [0,1,4,4,4,4,4]))

-- p1 force win
game11 :: Game
game11 = (One, ([0,0,0,0,0,1,10], [0,1,0,0,0,0,1]))

-- p2 force win
game12 :: Game
game12 = (Two, ([1,0,0,0,0,0,1], [10,1,0,0,0,0,0]))

-- Regression case: opponent's wells are empty
game13 :: Game
game13 = (One, ([0,0,0,0,0,0,25], [20,1,2,3,4,5,6]))

game14 :: Game
game14 = (One, ([1,0,0,2,1,0,19], [22,0,0,0,1,0,2]))

-- Strings for testing readGame
string1 :: String
string1 = "One\n4,4,4,4,4,4,0\n0,4,4,4,4,4,4"

string2 :: String
string2 = "Two\n4,4,4,4,4,4,0\n0,0,2,0,3,0,1"

string3 :: String
string3 = "One\n0,0,0,0,0,0,30\n5,0,0,0,0,0,0"

-- Compare actual and expected results
checkTest :: (Eq a, Show a) => String -> a -> a -> IO ()
checkTest name actual expected =
    if actual == expected
    then putStrLn (name ++ ": PASS")
    else putStrLn (name ++ ": FAIL\n  Expected: " ++ show expected ++ "\n  Got: " ++ show actual)

-- Find winner after making a move
winnerAfter :: Game -> Move -> Maybe Winner
winnerAfter game move =
    case makeMove game move of
        Nothing -> Nothing
        Just newGame -> checkWinner newGame

-- Run all tests
testAll :: IO ()
testAll = do
    putStrLn "\n--- LEGAL MOVES ---"
    checkTest "Legal Moves 1" (legalMoves game1) [0,1,2,3,4,5]
    checkTest "Legal Moves 2" (legalMoves game2) [0,2,4]
    checkTest "Legal Moves 3" (legalMoves game3) [2,4,6]

    putStrLn "\n--- CHECK WINNER ---"
    checkTest "Ongoing Game" (checkWinner game1) Nothing
    checkTest "Player 1 Wins" (checkWinner game4) (Just (Won One))
    checkTest "Player 2 Wins" (checkWinner game5) (Just (Won Two))
    checkTest "Draw" (checkWinner game6) (Just Draw)
    checkTest "Current Player Empty" (checkWinner game13) (Just (Won Two))

    putStrLn "\n--- MAKE MOVE ---"
    checkTest "Normal Move" (makeMove game1 0)
        (Just (Two, ([0,5,5,5,5,4,0], [0,4,4,4,4,4,4])))

    checkTest "Illegal Move" (makeMove game2 1)
        Nothing

    checkTest "Player 1 Capture" (makeMove game7 0)
        (Just (Two, ([0,0,0,0,2,0,5], [0,4,0,4,4,4,4])))

    checkTest "Player 1 Extra Turn" (makeMove game8 5)
        (Just (One, ([4,4,4,4,4,0,1], [0,4,4,4,4,4,4])))

    checkTest "Player 2 Capture" (makeMove game9 5)
        (Just (One, ([4,4,4,0,4,4,0], [5,0,0,0,0,0,1])))

    checkTest "Player 2 Extra Turn" (makeMove game10 1)
        (Just (Two, ([4,4,4,4,4,4,0], [1,0,4,4,4,4,4])))

    putStrLn "\n--- FUTURE WINNERS ---"
    checkTest "Player 1 Forced Win" (winnerAfter game11 5)
        (Just (Won One))

    checkTest "Player 2 Forced Win" (winnerAfter game12 1)
        (Just (Won Two))

    putStrLn "\n--- BEST MOVE CANDIDATES ---"
    checkTest "Player 1 Only Move" (legalMoves game11) [5]
    checkTest "Player 2 Only Move" (legalMoves game12) [1]

    putStrLn "\n--- STRING CONVERSIONS ---"
    checkTest "Read Game 1" (readGame string1) game1
    checkTest "Read Game 2" (readGame string2) game3
    checkTest "Read Game 3" (readGame string3) game4

    checkTest "Show Game 1" (showGame game1) string1
    checkTest "Show Game 2" (showGame game3) string2

    checkTest "String Round Trip" (readGame (showGame game7)) game7

    putStrLn "\n--- WHO WILL WIN ---"
    checkTest "Player 1 Forced Win" (whoWillWin game11)
        (Won One)

    checkTest "Player 2 Forced Win" (whoWillWin game12)
        (Won Two)

    checkTest "Multiple Possible Outcomes" (whoWillWin game14)
        (Won One)

    checkTest "Already Won" (whoWillWin game4)
        (Won One)

    checkTest "Already Draw" (whoWillWin game6)
        Draw

    putStrLn "\n--- BEST MOVE ---"
    checkTest "Player 1 Best Move" (bestMove game11)
        (Just 5)

    checkTest "Player 2 Best Move" (bestMove game12)
        (Just 1)

    checkTest "Multiple Possible Moves" (bestMove game14)
        (Just 4)

    checkTest "Game Already Finished" (bestMove game4)
        Nothing

    putStrLn "\n--- BEST MOVE WITH OUTCOME ---"
    checkTest "Player 1 Winning Move" (bestMoveWithOutcome game11)
        (Won One, Just 5)

    checkTest "Player 2 Winning Move" (bestMoveWithOutcome game12)
        (Won Two, Just 1)

    checkTest "Best Winning Strategy" (bestMoveWithOutcome game14)
        (Won One, Just 4)

    checkTest "Finished Game Outcome" (bestMoveWithOutcome game4)
        (Won One, Nothing)

    putStrLn "\n--- UPDATED FUNCTIONS ---"

    checkTest "Skip Player 2 Store"
        (advance One (Two, 1)) (One, 0)

    checkTest "Skip Player 1 Store"
        (advance Two (One, 5)) (Two, 6)

    checkTest "Increment Tile"
        (incAt [4,4,4,4,4,4,0] 2)
        [4,4,5,4,4,4,0]

    putStrLn "\n--- ALL TESTS COMPLETED ---"
