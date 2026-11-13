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