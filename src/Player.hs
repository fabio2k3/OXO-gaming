module Player where

import Board
import Text.Read (readMaybe)

-- Lectura segura de un entero en rango 0..2
readCoord :: String -> Maybe Int
readCoord s = do
    n <- readMaybe s :: Maybe Int
    if 0 <= n && n <= 2 then Just n else Nothing

-- Movimiento del usuario con validaciones
userMove :: Board -> IO Board
userMove board = do
    putStrLn "Tu turno. Ingresa fila (0-2):"
    rLine <- getLine
    case readCoord rLine of
        Nothing -> do
            putStrLn "Fila inválida. Debe ser 0, 1 o 2."
            userMove board
        Just r -> do
            putStrLn "Ingresa columna (0-2):"
            cLine <- getLine
            case readCoord cLine of
                Nothing -> do
                    putStrLn "Columna inválida. Debe ser 0, 1 o 2."
                    userMove board
                Just c ->
                    if board !! r !! c /= Empty
                        then do
                            putStrLn "Casilla ocupada. Intenta otra."
                            userMove board
                        else return (updateBoard board r c X)
