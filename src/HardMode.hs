module HardMode where

import Board
import System.Random (randomRIO)

-- Eliminar una casilla ocupada al azar (puede ser la del jugador o la de la IA)
randomlyRemove :: Board -> IO Board
randomlyRemove board = do
    let filled = [(r,c) | r <- [0..2], c <- [0..2], board !! r !! c /= Empty]
    if null filled
        then return board
        else do
            idx <- randomRIO (0, length filled - 1)
            let (r,c) = filled !! idx
            putStrLn $ "Se elimina casilla en (" ++ show r ++ "," ++ show c ++ ")"
            return (updateBoard board r c Empty)

-- Versión más estratégica (opcional): intenta eliminar una casilla del jugador que esté más cerca de completar línea.
-- Si no hay, elimina aleatoria. (Incluida por si quieres activarla más tarde.)
strategicRemove :: Board -> IO Board
strategicRemove board = do
    let playerCells = [(r,c) | r <- [0..2], c <- [0..2], board !! r !! c == X]
    -- buscar casilla del jugador que al eliminarla rompa su potencial de ganar.
    -- simplificación: si hay más de 1 casilla del jugador, eliminar una de sus casillas al azar
    if null playerCells
        then randomlyRemove board
        else do
            idx <- randomRIO (0, length playerCells - 1)
            let (r,c) = playerCells !! idx
            putStrLn $ "Se elimina casilla del jugador en (" ++ show r ++ "," ++ show c ++ ")"
            return (updateBoard board r c Empty)
