module AI where

import Board
import System.Random (randomRIO)

-- Movimiento aleatorio de la IA
aiMove :: Board -> IO Board
aiMove board = do
    let emptyCells = [(r,c) | r <- [0..2], c <- [0..2], board !! r !! c == Empty]
    if null emptyCells then return board
    else do
        idx <- randomRIO (0, length emptyCells - 1)
        let (r,c) = emptyCells !! idx
        return (updateBoard board r c O)
