import System.Random (randomRIO)
import Control.Monad
import Data.List (transpose)
import Data.Char (chr, ord)

type Board = [[Char]]
type Cell = (Int, Bool) -- (Valor, Bomba?)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  sizeTab <- getSizeTab
  let board = createBoard sizeTab
  sizeBomb <- getSizeBomb sizeTab
  newBoard <- generateBombs board sizeBomb
  printBoard newBoard
  playGame board sizeTab
  return()

-- Função para criar um tabuleiro
createBoard :: Int -> Board
createBoard sizeTab = replicate sizeTab (replicate sizeTab '*')

-- Função para imprimir o tabuleiro
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "Tabuleiro:"
  putStrLn "-----------------"
  mapM_ printRow (map (map cellToChar) board)
  putStrLn "-----------------"

printRow :: [Char] -> IO ()
printRow row = putStrLn row

cellToChar :: Char -> Char
cellToChar '+' = '*'
cellToChar c = c

-- Função para obter o tamanho do tabuleiro a partir do usuário
getSizeTab :: IO Int
getSizeTab = do
  putStrLn "Informe o tamanho 'n' do campo (entre 3 e 10, campo n x n):"
  sizeTab <- readLn
  if sizeTab >= 3 && sizeTab <= 10
    then return sizeTab
    else do
      putStrLn "Tamanho inválido. Por favor informar tamanho entre 3 e 10."
      getSizeTab

-- Função para obter a quantidade de bombas a partir do usuário
getSizeBomb :: Int -> IO Int
getSizeBomb sizeTab = do
  putStrLn "Informe a quantidade de bombas do campo:"
  sizeBomb <- readLn
  let sizeTabTo = sizeTab * sizeTab
  if sizeBomb <= sizeTabTo `div` 2
    then return sizeBomb
    else do
      putStrLn "Quantidade inválida de bombas, tente novamente"
      getSizeBomb sizeTab


-- Função para gerar aleatoriamente as posições das bombas de forma que não se repita, 
-- seguida de funções auxiliares para atualização da tabela, que é retornada
generateBombs :: Board -> Int -> IO Board
generateBombs board numBombs = do
  let sizeTab = length board
      sizeTabTo = sizeTab * sizeTab
  positions <- generateUniquePositions numBombs sizeTabTo
  let updatedBoard = foldl (\acc pos -> updateBoard pos '+' acc) board positions
  return updatedBoard

generateUniquePositions :: Int -> Int -> IO [Int]
generateUniquePositions numPositions range = do
  positions <- generateUniquePositions' numPositions range []
  return (take numPositions positions)

generateUniquePositions' :: Int -> Int -> [Int] -> IO [Int]
generateUniquePositions' 0 _ acc = return acc
generateUniquePositions' numPositions range acc = do
  position <- randomRIO (0, range - 1)
  if position `elem` acc
    then generateUniquePositions' numPositions range acc
    else generateUniquePositions' (numPositions - 1) range (position : acc)

updateBoard :: Int -> Char -> Board -> Board
updateBoard pos char board =
  let (row, col) = indexToPosition pos (length board)
      (upperRows, currentRow:lowerRows) = splitAt row board
      updatedRow = updateList col char currentRow
   in upperRows ++ (updatedRow : lowerRows)

updateList :: Int -> a -> [a] -> [a]
updateList _ _ [] = []
updateList index newVal (x:xs)
  | index == 0 = newVal : xs
  | otherwise = x : updateList (index - 1) newVal xs

indexToPosition :: Int -> Int -> (Int, Int)
indexToPosition index size =
  let row = index `div` size
      col = index `mod` size
  in (row, col)

-- Tela principal do jogo
playGame :: [[Char]] -> Int -> IO()
playGame board sizeTab = do
  putStrLn "Selecione uma das opções abaixo:"
  putStrLn "M seguido de linha e coluna para marcar uma posição"
  putStrLn "D seguido de linha e coluna para desmarcar uma posição"
  putStrLn "A seguido de linha e coluna para abrir uma posição"
  entry <- getLine
  let action = head entry -- primeiro caractere (A, M ou D)
      params = tail entry -- restante da string (linha e coluna)
      row = read [head params] :: Int -- converte o primeiro caractere em um valor inteiro
      col = read [last params] :: Int -- converte o último caractere em um valor inteiro
  if action == 'M' && row <= sizeTab && col <= sizeTab then
    -- implementação para marcar posição
  else if action == 'D' && row <= sizeTab && col <= sizeTab then
    -- implementação para desmarcar posição
  else if action == 'A' && row <= sizeTab && col <= sizeTab then
    -- implementação para acessar posição
  else
    putStrLn "Entrada incorreta, informe a letra M, D ou A seguida da linha e coluna na qual você queira fazer uma ação"
    putStrLn "Exemplos: M21 para marcar linha 2, coluna 1"
    putStrLn "          D34 para desmarcar linha 3, coluna 4"
    putStrLn "          A11 para acessar linha 1, coluna 1"
    playGame board sizeTab
  return()