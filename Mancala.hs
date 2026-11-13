import Data.Maybe

type Game = (Player, Board) --Player whose turn it is
type Board = ([Tile], [Tile])
--fst is Player 1's side, of form [Man,  ... , Cala]
--snd is Player 2's side, of form [Cala, ... , Man]
data Player = One | Two deriving (Show, Eq)
type Tile = Int -- # of marbles in tile
type Move = Int -- index of Man moved on

checkWinner :: Game -> Maybe Player
checkWinner (_, (p1, p2)) = 
    let score1 = sum p1
        score2 = sum p2
    in  if(score1 == score2) then Nothing
        else if(score1>score2) then Just One else Just Two

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

-- Returns the list containing the indices of legal moves.
-- Does not check whether a board configuration is valid.
-- Note that these indices correspond to the list of tiles that for a particular player's side,
-- not to the board in general. 
-- Examples: 
-- legalMoves (One, ([-5000, 003 ,0, 52000, 2, 8, 7432], [34322, 0, -01, 300, 8, 123, 3])) = [1, 3, 4, 5]
-- legalMoves (Two, ([-5000, 003 ,0, 52000, 2, 8, 7432], [34322, 0, -01, 300, 8, 123, 3])) = [3, 4, 5, 6]
legalMoves :: Game -> [Move]
legalMoves (One, (p1Tiles, _)) = indicesOfValid 0 (init p1Tiles)
legalMoves (Two, (_, p2Tiles)) = map (\i -> i+1) (indicesOfValid 0 (drop 1 p2Tiles)) -- The map fixes the offset cause by dropping the first element

indicesOfValid :: Int -> [Int] -> [Int]
indicesOfValid index [] = []
indicesOfValid index (x:xs) = if x > 0 then index:(indicesOfValid (index+1) xs) else (indicesOfValid (index+1) xs)
