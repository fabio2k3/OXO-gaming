module Player where

import Board

-- Movimiento del usuario
userMove :: Board -> IO Board
userMove board = do
    putStrLn "Ingresa fila (0-2):"
    r <- getLine
    putStrLn "Ingresa columna (0-2):"
    c <- getLine
    let row = read r
        col = read c
    if board !! row !! col == Empty
        then return (updateBoard board row col X)
        else do
            putStrLn "Casilla ocupada. Intenta de nuevo."
            userMove board
