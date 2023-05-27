import System.Random (randomRIO)
import Data.List (transpose)

type Board = [[Cell]]
type Cell = (Int, Bool) -- (Valor, Bomba?)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  size <- getSize
  bombs <- generateBombs size
  let board = generateBoard size bombs
  playGame board

-- Função para obter o tamanho do tabuleiro a partir do usuário
getSize :: IO Int
getSize = do
  putStrLn "Informe o tamanho do tabuleiro (entre 3 e 10):"
  size <- readLn
  if size >= 3 && size <= 10
    then return size
    else do
      putStrLn "Tamanho inválido. Por favor, tente novamente."
      getSize

-- Função para gerar aleatoriamente as posições das bombas
generateBombs :: Int -> IO [(Int, Int)]
generateBombs size = do
  positions <- sequence [randomRIO (0, size - 1) >>= \x -> randomRIO (0, size - 1) >>= \y -> return (x, y) | _ <- [1..size]]
  return positions

-- Função para gerar o tabuleiro inicial
generateBoard :: Int -> [(Int, Int)] -> Board
generateBoard size bombs =
  let
    board = replicate size (replicate size (0, False))
    boardWithBombs = foldl (\acc (x, y) -> updateCell acc (x, y) (0, True)) board bombs
  in
    updateCells boardWithBombs

-- Função para atualizar o valor das células adjacentes às bombas
updateCells :: Board -> Board
updateCells board = transpose $ map (map updateCellValue) (transpose $ map (map (\cell -> if snd cell then cell else updateCellValue cell)) board)

-- Função para atualizar o valor de uma célula
updateCellValue :: Cell -> Cell
updateCellValue (value, bomb) = (value + countAdjacentBombs, bomb)
  where
    countAdjacentBombs = if bomb then 0 else length $ filter (\(_, isBomb) -> isBomb) $ getAdjacentCells (value, bomb)

-- Função para obter as células adjacentes a uma dada célula
getAdjacentCells :: Cell -> [Cell]
getAdjacentCells (value, bomb) = [
  (value, bomb), (value, bomb), (value, bomb), (value, bomb), -- Células acima, abaixo, à esquerda e à direita
  (value, bomb), (value, bomb), (value, bomb), (value, bomb)  -- Células diagonais
  ]

-- Função para atualizar uma célula no tabuleiro
updateCell :: Board -> (Int, Int) -> Cell -> Board
updateCell board (x, y) cell = take x board ++ [take y (board !! x) ++ [cell] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board

-- Função para exibir o tabuleiro atual
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "Tabuleiro:"
  mapM_ printRow board
  putStrLn ""

-- Função para exibir uma linha do tabuleiro
printRow :: [Cell] -> IO ()
printRow row = do
  mapM_ printCell row
  putStrLn ""

-- Função para exibir uma célula do tabuleiro
printCell :: Cell -> IO ()
printCell (value, bomb)
  | bomb = putStr "[*]"
  | otherwise = putStr ("[" ++ show value ++ "]")

-- Função principal do jogo
playGame :: Board -> IO ()
playGame board = do
  printBoard board
  putStrLn "Selecione a posição (linha, coluna):"
  (x, y) <- readLn
  let cell = (board !! x) !! y
  if snd cell
    then putStrLn "Você perdeu! Game over."
    else do
      let updatedBoard = updateCell board (x, y) cell
      playGame updatedBoard
