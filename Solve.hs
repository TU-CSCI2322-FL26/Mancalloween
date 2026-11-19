module Solve where

import Mancala
import Data.Maybe

whoWillWin :: Game -> Winner
whoWillWin g = 
    case (checkWinner g) of 
        Just w -> w
        Nothing -> aux g
    where 
        aux g@(p,_) = aux' (map (fromJust) (map (makeMove g) (legalMoves g))) (Won $ otherPlayer p)
            where aux' [] bestOutcome = bestOutcome
                  aux' (g:gs) bestOutcome =
                    let o = whoWillWin g
                    in  if o == (Won p) then Won p
                        else if o == Draw then (aux' gs Draw)
                        else (aux' gs bestOutcome)
