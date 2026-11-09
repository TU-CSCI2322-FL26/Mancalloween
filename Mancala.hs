type Game = (Player, Board) --Player whose turn it is
type Board = ([Tile], [Tile])
--fst is Player 1's side, of form [Man,  ... , Cala]
--snd is Player 2's side, of form [Cala, ... , Man]
data Player = One | Two deriving (Show, Eq)
type Tile = Int -- # of marbles in tile
type Move = Int -- index of Man moved on


-- Prints a game
-- Example Usage:
-- putStrLn $ prettyPrint (One, ([-5000, 003 ,4, 52000, 2, 8, 7], [32, 0, -01, 300, 2, 1, 3]))
-- putStrLn $ prettyPrint (One, ([-5000, 003 ,0, 52000, 2, 8, 7432], [34322, 0, -01, 300, 8, 123, 3]))
prettyPrint :: Game -> String
prettyPrint (turn, board@(b:bs, t:ts))
    | length (b:bs) /= length (t:ts) = "Inputs are of unequal length!"
    | otherwise = (topRow ++ "\n" ++ centerRow ++ "\n" ++ bottomRow)
    where
        calaR = show $ last $ b:bs
        calaL = show $ t
        pad = [' ' | char <- calaL]
        
        bottomRow = pad ++ " |" ++ (init . init $ fst rows) ++ "| "
        topRow = pad ++ " |" ++ (init . init $ snd rows) ++ "| "
        
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

