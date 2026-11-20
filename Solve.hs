module Solve where

import Mancala
import Data.Maybe

whoWillWin :: Game -> Winner
whoWillWin g@(p,_) = 
    case (checkWinner g) of 
        Just w -> w
        Nothing -> aux (map (fromJust) (map (makeMove g) (legalMoves g))) (Won $ otherPlayer p)
        where aux [] bestOutcome = bestOutcome
              aux (g:gs) bestOutcome =
                let o = whoWillWin g
                in  if o == (Won p) then Won p
                    else if o == Draw then (aux gs Draw)
                    else (aux gs bestOutcome)

bestMove :: Game -> Maybe Move
bestMove g = snd $ bestMoveWithOutcome g

bestMoveWithOutcome :: Game -> (Winner, Maybe Move)
bestMoveWithOutcome g@(p,_) = 
    case (checkWinner g) of
        Just w -> (w, Nothing)
        Nothing -> 
            let (o,m) = aux (legalMoves g) (Won $ otherPlayer p) (-1)
            in  (o, Just m)
        where aux [] bestO bestM = (bestO, bestM)
              aux (m:ms) bestO bestM =
                let (o,_) = bestMoveWithOutcome (fromJust $ makeMove g m)
                in  if o == Won p then (o, m)
                    else if o == Draw then aux ms o m
                    else if bestM /= -1 then aux ms bestO bestM
                    else aux ms o m