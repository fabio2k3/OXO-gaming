module Main where

import Board
import Player
import AI
import HardMode
import Data.Maybe (isJust)

-- Juego: turno usuario -> turno IA -> (si dificil) eliminar -> repetir
gameLoop :: Board -> String -> IO ()
gameLoop board mode = do
    printBoard board

    -- revisar ganador o empate antes de pedir movimiento
    case checkWin board of
        Just X -> putStrLn "¡Ganaste!" >> printBoard board
        Just O -> putStrLn "La IA gana 😢" >> printBoard board
        Nothing -> if isFull board
                      then putStrLn "Empate. Tablero lleno." >> printBoard board
                      else do
                          -- turno usuario
                          board' <- userMove board
                          printBoard board'
                          case checkWin board' of
                              Just X -> putStrLn "¡Ganaste!" >> printBoard board'
                              Just O -> putStrLn "La IA gana 😢" >> printBoard board'
                              Nothing -> if isFull board'
                                  then putStrLn "Empate. Tablero lleno." >> printBoard board'
                                  else do
                                      -- turno IA (mejorada)
                                      putStrLn "Turno de la IA..."
                                      board'' <- aiMove board'
                                      printBoard board''
                                      case checkWin board'' of
                                          Just X -> putStrLn "¡Ganaste!" >> printBoard board''
                                          Just O -> putStrLn "La IA gana 😢" >> printBoard board''
                                          Nothing -> do
                                              boardFinal <- if mode == "dificil"
                                                  then randomlyRemove board''
                                                  else return board''
                                              gameLoop boardFinal mode

main :: IO ()
main = do
    putStrLn "Bienvenido a Cero-Cruz"
    putStrLn "Elige modo: normal / dificil"
    mode <- getLine
    let mode' = if mode == "dificil" then "dificil" else "normal"
    gameLoop emptyBoard mode'
