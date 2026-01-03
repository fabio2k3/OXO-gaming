module Main where

import Board
import Player
import AI
import HardMode

gameLoop :: Board -> String -> IO ()
gameLoop board mode = do
    printBoard board
    case checkWin board of
        Just "Usuario" -> putStrLn "¡Ganaste!"
        Just "IA" -> putStrLn "La IA gana 😢"
        Nothing -> do
            board' <- userMove board
            board'' <- aiMove board'
            finalBoard <- if mode == "dificil"
                then randomlyRemove board''
                else return board''
            gameLoop finalBoard mode

main :: IO ()
main = do
    putStrLn "Bienvenido a Cero-Cruz"
    putStrLn "Elige modo: normal / dificil"
    mode <- getLine
    gameLoop emptyBoard mode
