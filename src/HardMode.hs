module HardMode where

import Board
import System.Random (randomRIO)

-- Eliminar una casilla aleatoria
randomlyRemove :: Board -> IO Board
randomlyRemove board = do
    let filledCells = [(r,c) | r <- [0..2], c <- [0..2], board !! r !! c /= Empty]
    if null filledCells then return board
    else do
        idx <- randomRIO (0, length filledCells - 1)
        let (r,c) = filledCells !! idx
        putStrLn $ "Se elimina casilla en (" ++ show r ++ "," ++ show c ++ ")"
        return (updateBoard board r c Empty)
