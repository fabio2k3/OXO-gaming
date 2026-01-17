module Board where

import Data.List (transpose)

-- Tipo de celda y alias de Board
data Cell = X | O | Empty deriving (Eq, Show)
type Board = [[Cell]]

-- Tablero vacío 3x3
emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Empty)

-- Mostrar tablero en consola
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "   0 1 2"
    mapM_ printRow (zip [0..] board)
  where
    printRow (i, row) = putStrLn $ show i ++ "  " ++ unwords (map showCell row)
    showCell Empty = "_"
    showCell X     = "X"
    showCell O     = "O"

-- Actualizar tablero (inmutable)
updateBoard :: Board -> Int -> Int -> Cell -> Board
updateBoard board r c val =
    take r board ++
    [take c (board !! r) ++ [val] ++ drop (c+1) (board !! r)] ++
    drop (r+1) board

-- Todas las casillas vacías
emptyCells :: Board -> [(Int, Int)]
emptyCells board = [(r,c) | r <- [0..2], c <- [0..2], board !! r !! c == Empty]

-- ¿Tablero lleno?
isFull :: Board -> Bool
isFull board = null (emptyCells board)

-- Comprobar ganador: devuelve Just X o Just O, o Nothing
checkWin :: Board -> Maybe Cell
checkWin b
    | any (all (== X)) lines = Just X
    | any (all (== O)) lines = Just O
    | otherwise              = Nothing
  where
    rows = b
    cols = transpose b
    dias = [[b !! i !! i | i <- [0..2]],
            [b !! i !! (2-i) | i <- [0..2]]]
    lines = rows ++ cols ++ dias
