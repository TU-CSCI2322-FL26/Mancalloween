module Main where
import Mancala
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Debug.Trace

-- Prints a game
-- Example Usage:
-- putStrLn $ prettyPrint (One, ([-5000, 003 ,4, 52000, 2, 8, 7], [32, 0, -01, 300, 2, 1, 3]))
-- putStrLn $ prettyPrint (One, ([-5000, 003 ,0, 52000, 2, 8, 7432], [34322, 0, -01, 300, 8, 123, 3]))
prettyPrint :: Game -> String
prettyPrint (turn, board@(b:bs, t:ts))
    | length (b:bs) /= length (t:ts) = "Inputs are of unequal length!"
    | otherwise = ("Player " ++ show turn ++ " to move!\n" ++ topRow ++ "\n" ++ centerRow ++ "\n" ++ bottomRow)
    where
        calaR = show $ last $ b:bs
        calaL = show $ t
        pad = [' ' | char <- calaL]

        bottomRow = pad ++ " |" ++ (init . init $ snd rows) ++ "| "
        topRow = pad ++ " |" ++ (init . init $ fst rows) ++ "| "

        rows = prettyPrintRows (drop 1 $ snd board) (init $ fst board)
        centerRow = 
            calaL ++ " |" ++ 
            [' ' | _ <- [1..((length $ fst $ rows)-2)]] ++ 
            "| " ++ calaR

-- Takes in two lists of tiles and returns the string representations of the bottom and top rows (in that order)
-- Ensures pretty printing works with multidigit numbers
prettyPrintRows :: [Tile] -> [Tile] -> (String, String)
prettyPrintRows [] [] = ("", "")
prettyPrintRows bottom@(b:bs) top@(t:ts)
    | length top /= length bottom = error "Inputs are of unequal length!"
    | otherwise = (bStr ++ ", " ++ fst recurse, tStr ++ ", " ++ snd recurse)
        where
            bStr = [' ' | _ <- [1..(maxIntLength - (length $ show b))]] ++ show b
            tStr = [' ' | _ <- [1..(maxIntLength - (length $ show t))]] ++ show t
            maxIntLength = max (length $ show b) (length $ show t)
            recurse = prettyPrintRows bs ts


-- Game file format example:
--One                   <- Player to move
--2,1,3,4,5,6           <- PLayer1 Rows [Man, ... , Cala]
--6,4,7,2,1,0           <- Player2 Rows [Cala, ..., Man]

readGame :: String -> Game
readGame gameStr = (player, (r1, r2))
    where 
        [p, p1Rows, p2Rows] = lines gameStr
        player = if p == "One" then One else Two
        r1 = map (\e -> read e) (splitOn "," p1Rows)
        r2 = map (\e -> read e) (splitOn "," p2Rows)

showGame :: Game -> String
showGame (player, (p1Rows, p2Rows)) = show player ++ "\n" ++ l1 ++ "\n" ++ l2
    where
        l1 = drop 1 $ init $ show p1Rows
        l2 = drop 1 $ init $ show p2Rows

writeGame :: Game -> FilePath -> IO ()
writeGame game filePath = do
    let gameStr = showGame game
    writeFile filePath gameStr

loadGame :: FilePath -> IO Game
loadGame filePath = do
    gameStr <- readFile filePath
    let game = readGame gameStr
    return game

-- To be adjusted after bestMove is completed
-- Implement the forced outcome later on
putBestMove :: Game -> IO ()
putBestMove game = do
    let player = fst game
    let bestMove = show $ (-1) --bestMove game
    putStrLn ("Best move is player " ++ show player ++ " on " ++ bestMove ++ " to force a ...")

--
main :: IO ()
main = do
    putStr "File Path?: "
    filePath <- getLine
    game <- loadGame filePath
    putBestMove game 