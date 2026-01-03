module Board where

import Data.List (transpose)

data Cell = X | O | Empty deriving (Eq, Show)
type Board = [[Cell]]

-- Tablero vacío
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Empty)

-- Imprimir tablero
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "  0 1 2"
    mapM_ printRow (zip [0..] board)
  where
    printRow (i, row) = putStrLn $ show i ++ " " ++ unwords (map showCell row)
    showCell Empty = "_"
    showCell X = "X"
    showCell O = "O"

-- Actualizar tablero
updateBoard :: Board -> Int -> Int -> Cell -> Board
updateBoard board row col val =
    take row board ++
    [take col (board !! row) ++ [val] ++ drop (col + 1) (board !! row)] ++
    drop (row + 1) board

-- Verificar ganador
checkWin :: Board -> Maybe String
checkWin board
    | any (all (== X)) (rows ++ cols ++ dias) = Just "Usuario"
    | any (all (== O)) (rows ++ cols ++ dias) = Just "IA"
    | otherwise = Nothing
  where
    rows = board
    cols = transpose board
    dias = [[board !! i !! i | i <- [0..2]],
            [board !! i !! (2-i) | i <- [0..2]]]
