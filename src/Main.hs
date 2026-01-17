module Main where

import Board
import Player
import AI
import HardMode
import Data.Maybe (isJust)
import System.Random (randomRIO)

-- Función que revisa fin de juego y termina con mensaje
checkEndGame :: Board -> IO Bool
checkEndGame board = do
    case checkWin board of
        Just X -> putStrLn "¡Ganaste!" >> printBoard board >> return True
        Just O -> putStrLn "La IA gana 😢" >> printBoard board >> return True
        Nothing -> if isFull board
                      then putStrLn "Empate. Tablero lleno." >> printBoard board >> return True
                      else return False

-- Turno completo (usuario y IA)
gameLoop :: Board -> String -> Bool -> IO ()
gameLoop board mode userStarts = do
    -- Verificar fin de juego antes de cualquier movimiento
    end <- checkEndGame board
    if end
        then putStrLn "Juego terminado."
        else do
            if userStarts
                then do
                    -- Turno usuario
                    board' <- userMove board
                    printBoard board'
                    end' <- checkEndGame board'
                    if end'
                        then putStrLn "Juego terminado."
                        else do
                            -- Turno IA
                            putStrLn "Turno de la IA..."
                            board'' <- aiMove board'
                            printBoard board''
                            boardFinal <- if mode == "dificil"
                                then randomlyRemove board''
                                else return board''
                            gameLoop boardFinal mode userStarts
                else do
                    -- Turno IA primero
                    putStrLn "Turno de la IA..."
                    board'' <- aiMove board
                    printBoard board''
                    boardAfterIA <- if mode == "dificil"
                        then randomlyRemove board''
                        else return board''
                    end' <- checkEndGame boardAfterIA
                    if end'
                        then putStrLn "Juego terminado."
                        else do
                            -- Turno usuario
                            board''' <- userMove boardAfterIA
                            printBoard board'''
                            gameLoop board''' mode userStarts

-- Piedra-Papel-Tijeras para decidir quién empieza
decideFirstTurn :: IO Bool
decideFirstTurn = do
    putStrLn "Elige tu opción para ver quién empieza: piedra / papel / tijeras"
    userChoice <- getLine
    let validChoices = ["piedra","papel","tijeras"]
    if userChoice `notElem` validChoices
       then do
           putStrLn "Opción inválida."
           decideFirstTurn
       else do
           idx <- randomRIO (0,2)
           let iaChoice = validChoices !! idx
           putStrLn $ "La IA eligió: " ++ iaChoice
           let winner = case (userChoice, iaChoice) of
                   ("piedra","tijeras")   -> "usuario"
                   ("tijeras","papel")    -> "usuario"
                   ("papel","piedra")     -> "usuario"
                   ("tijeras","piedra")   -> "ia"
                   ("papel","tijeras")    -> "ia"
                   ("piedra","papel")     -> "ia"
                   _                       -> "empate"
           case winner of
               "usuario" -> do
                   putStrLn "¡Ganaste el piedra-papel-tijeras! Empiezas primero."
                   return True
               "ia" -> do
                   putStrLn "La IA ganó el piedra-papel-tijeras. Ella empieza."
                   return False
               "empate" -> do
                   putStrLn "Empate, intentemos de nuevo."
                   decideFirstTurn

main :: IO ()
main = do
    putStrLn "Bienvenido a Cero-Cruz"
    putStrLn "Elige modo: normal / dificil"
    modeInput <- getLine
    let mode = if modeInput == "dificil" then "dificil" else "normal"
    userStarts <- decideFirstTurn
    gameLoop emptyBoard mode userStarts
