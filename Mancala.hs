import Data.Maybe

type Game = (Player, Board) --Player whose turn it is
type Board = ([Tile], [Tile])
--fst is Player 1's side, of form [Man,  ... , Cala]
--snd is Player 2's side, of form [Cala, ... , Man]
data Player = One | Two deriving (Show, Eq)
data Winner = Won Player | Draw deriving (Show, Eq)
type Tile = Int -- # of marbles in tile
type Move = Int -- index of Man moved on

type Position = (Player, Int)

checkWinner :: Game -> Maybe Player
checkWinner (_, (p1, p2)) = 
checkWinner :: Game -> Maybe Winner
checkWinner (One, (p1,p2)) = if(any (/= 0) (take 6 p1)) then Nothing else Just $ evalScoresForWinner (One, (p1,p2))
checkWinner (Two, (p1,p2)) = if(any (/= 0) (drop 1 p2)) then Nothing else Just $ evalScoresForWinner (Two, (p1,p2))

evalScoresForWinner :: Game -> Winner
evalScoresForWinner (_, (p1, p2)) = 
    let score1 = sum p1
        score2 = sum p2
    in  if(score1 == score2) then Nothing
        else if(score1>score2) then Just One else Just Two
    in  if(score1 == score2) then Draw
        else if(score1>score2) then Won One else Won Two

-- Update tile at index
updateTile :: [Tile] -> Int -> Int -> [Tile]
updateTile [] _ _ = []
updateTile (x:xs) 0 value = value : xs
updateTile (x:xs) index value =
    x : updateTile xs (index - 1) value

-- Get num marbles at position
getTile :: Position -> Board -> Int
getTile (One, i) (p1, p2) = p1 !! i
getTile (Two, i) (p1, p2) = p2 !! i

-- Change num marbles at position
setTile :: Position -> Int -> Board -> Board
setTile (One, i) value (p1, p2) =
    (updateTile p1 i value, p2)
setTile (Two, i) value (p1, p2) =
    (p1, updateTile p2 i value)

otherPlayer :: Player -> Player
otherPlayer One = Two
otherPlayer Two = One

-- calas for each player
storeIndex :: Player -> Int
storeIndex One = 6
storeIndex Two = 0

-- Find the next position on board
nextPosition :: Position -> Position
nextPosition (One, 6) = (Two, 6)
nextPosition (One, i) = (One, i + 1)
nextPosition (Two, 0) = (One, 0)
nextPosition (Two, i) = (Two, i - 1)

-- Skip the opponent Cala and keep going
advance :: Player -> Position -> Position
advance player pos =
    let next = nextPosition pos
    in if next == (otherPlayer player,
                   storeIndex (otherPlayer player))
       then nextPosition next
       else next

-- mmm marbel
addMarble :: Position -> Board -> Board
addMarble pos board =
    setTile pos (getTile pos board + 1) board

-- recurse marbles
sow :: Player -> Position -> Int -> Board -> (Board, Position)
sow player pos 0 board = (board, pos)
sow player pos marbles board =
    let next = advance player pos
        newBoard = addMarble next board
    in sow player next (marbles - 1) newBoard

-- special move
capture :: Player -> Position -> Board -> Board
capture player pos@(side, i) board
    | side /= player = board
    | i == storeIndex player = board
    | getTile pos board /= 1 = board
    | getTile opposite board == 0 = board
    | otherwise =
        let amount = 1 + getTile opposite board
            cleared = setTile pos 0
                      (setTile opposite 0 board)
            store = (player, storeIndex player)
        in setTile store
             (getTile store cleared + amount) cleared
    where
        opposite = case player of
            One -> (Two, i + 1)
            Two -> (One, i - 1)

-- do a move wit all the things
makeMove :: Game -> Move -> Game
makeMove (player, board) move =
    let start = (player, move)
        -- pick up marbles
        marbles = getTile start board
        -- Eempty pit
        emptyBoard = setTile start 0 board
        -- move marbles
        (newBoard, lastPos) =
            sow player start marbles emptyBoard
        -- capture
        finalBoard = capture player lastPos newBoard
        -- next moveman
        nextPlayer =
            if lastPos == (player, storeIndex player)
            then player
            else otherPlayer player
    in (nextPlayer, finalBoard)

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