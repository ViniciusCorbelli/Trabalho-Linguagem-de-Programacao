import System.Random (randomRIO)
import Data.List (transpose)
import Data.Char (chr, ord)

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
updateCellValue (value, bomb) = if bomb then (value, bomb) else (countAdjacentBombs, bomb)
  where
    countAdjacentBombs = length $ filter (\(_, isBomb) -> isBomb) $ getAdjacentCells (value, bomb)

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
  let columnLetters = take (length board) ['A'..]
  putStrLn $ "   " ++ unwords (map (\c -> [c]) columnLetters) -- Imprime as letras das colunas
  mapM_ printRow (zip [1..] board)
  putStrLn ""

-- Função para exibir uma linha do tabuleiro
printRow :: (Int, [Cell]) -> IO ()
printRow (rowNum, row) = do
  putStr (padLeft 2 (show rowNum) ++ " ")
  mapM_ printCell row
  putStrLn ""

-- Função para exibir uma célula do tabuleiro
printCell :: Cell -> IO ()
printCell (_, bomb) | bomb = putStr "[ ]"
printCell (value, _) | value > 0 = putStr ("[" ++ show value ++ "]")
printCell _ = putStr "[ ]"

-- Função principal do jogo
playGame :: Board -> IO ()
playGame board = do
  printBoard board
  putStrLn "Selecione a posição (linha, coluna):"
  position <- getLine
  let (x, y) = parsePosition position
  let cell = (board !! x) !! y
  if snd cell
    then putStrLn "Você perdeu! Game over."
    else do
      let updatedBoard = updateCell board (x, y) cell
      playGame updatedBoard

-- Função para converter a posição do formato "1A" para as coordenadas (linha, coluna)
parsePosition :: String -> (Int, Int)
parsePosition position =
  let
    rowNumberStr = takeWhile (/= 'a') position
    rowNumber = read rowNumberStr :: Int
    columnLetter = last position
    column = ord (toUpper columnLetter) - ord 'A'
    row = rowNumber - 1
  in
    (row, column)

-- Função para preencher uma string à esquerda com espaços para ter um comprimento específico
padLeft :: Int -> String -> String
padLeft len str = replicate (len - length str) ' ' ++ str

-- Função para converter um caractere em letra maiúscula
toUpper :: Char -> Char
toUpper c
  | c >= 'a' && c <= 'z' = chr (ord c - ord 'a' + ord 'A')
  | otherwise = c
