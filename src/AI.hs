module AI where

import Board
import System.Random (randomRIO)
import Data.List (intersect)

-- Intenta encontrar un movimiento que haga ganar a 'player' inmediatamente.
-- Devuelve Just (r,c) o Nothing.
findWinningMove :: Board -> Cell -> Maybe (Int, Int)
findWinningMove board player =
    let empties = emptyCells board
        wins = filter (\(r,c) -> checkWin (updateBoard board r c player) == Just player) empties
    in case wins of
        (p:_) -> Just p
        []    -> Nothing

-- Prioridad: centro, esquinas, lados. Si hay varias esquinas disponibles, selecciona una aleatoria.
priorityMove :: Board -> IO (Int, Int)
priorityMove board = do
    let empt = emptyCells board
        center = [(1,1)]
        corners = [(0,0),(0,2),(2,0),(2,2)]
        sides   = [(0,1),(1,0),(1,2),(2,1)]
        available xs = filter (`elem` empt) xs
    -- Si centro libre, tomarlo
    if not (null (available center)) then return (1,1)
    else do
        let availCorners = available corners
        if not (null availCorners)
            then do idx <- randomRIO (0, length availCorners - 1)
                    return (availCorners !! idx)
            else do
                let availSides = available sides
                if not (null availSides)
                    then do idx <- randomRIO (0, length availSides - 1)
                            return (availSides !! idx)
                    else error "priorityMove: no moves available"

-- Movimiento de IA mejorado:
-- 1) Si puede ganar ahora, gana.
-- 2) Si el oponente puede ganar en el siguiente turno, bloquea.
-- 3) Si no, prioriza centro/esquinas/lados (con algo de aleatoriedad en esquinas/sides).
-- 4) Si nada (muy raro), elige aleatorio entre vacíos.
aiMove :: Board -> IO Board
aiMove board = do
    let maybeWin = findWinningMove board O      -- O es la IA
        maybeBlock = findWinningMove board X    -- X es el jugador (hay que bloquear)
        empt = emptyCells board

    (r,c) <- case maybeWin of
        Just p -> return p
        Nothing -> case maybeBlock of
            Just p2 -> return p2
            Nothing -> if null empt
                        then error "aiMove: no empty cells"
                        else priorityMove board

    return (updateBoard board r c O)
