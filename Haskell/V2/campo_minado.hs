import System.Random (randomRIO)
import Control.Monad
import Data.List (transpose, intersperse)
import Data.Char (chr, ord, intToDigit)
import Data.Bool (Bool(..), (||))

type Board = [[Cell]]
type Cell = (Char, Bool) -- (Conteúdo, Bomba?)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  sizeTab <- getSizeTab
  let board = createBoard sizeTab
  sizeBomb <- getSizeBomb sizeTab
  newBoard <- generateBombs board sizeBomb
  printBoard newBoard
  playGame newBoard sizeTab sizeBomb
  return()

-- Função para criar um tabuleiro
createBoard :: Int -> Board
createBoard sizeTab = replicate sizeTab (replicate sizeTab ('*', False))

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

-- Função para imprimir o tabuleiro (escondendo as bombas)
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "Tabuleiro:"
  putStrLn "-----------------"
  let indexedBoard = zip [1..] board
  printColumnLetters (length (head board))
  mapM_ printIndexedRow (reverse indexedBoard)
  putStrLn "-----------------"

printColumnLetters :: Int -> IO ()
printColumnLetters numCols = do
  let letters = take numCols ['A'..]
  putStrLn $ "  " ++ intersperse ' ' letters  -- Espaçamento e letras das colunas

printIndexedRow :: (Int, [Cell]) -> IO ()
printIndexedRow (index, row) = do
  let convertedRow = map cellToChar row
  putStrLn (show index ++ " " ++ intersperse ' ' convertedRow)

cellToChar :: Cell -> Char
cellToChar (c, _) = c

-- Função para gerar aleatoriamente as posições das bombas de forma que não se repita, 
-- seguida de funções auxiliares para atualização da tabela, que é retornada
generateBombs :: Board -> Int -> IO Board
generateBombs board numBombs = do
  let sizeTab = length board
      sizeTabTo = sizeTab * sizeTab
  positions <- generateUniquePositions numBombs sizeTabTo
  let updatedBoard = foldl (\acc pos -> updateBoard pos ('*', True) acc) board positions
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

updateBoard :: Int -> Cell -> Board -> Board
updateBoard pos cell board =
  let (row, col) = indexToPosition pos (length board)
      (upperRows, currentRow:lowerRows) = splitAt row board
      updatedRow = updateList col cell currentRow
   in upperRows ++ (updatedRow : lowerRows)

updateList :: Int -> Cell -> [Cell] -> [Cell]
updateList _ _ [] = []
updateList index newVal (x:xs)
  | index == 0 = newVal : xs
  | otherwise = x : updateList (index - 1) newVal xs

indexToPosition :: Int -> Int -> (Int, Int)
indexToPosition index size =
  let row = index `div` size
      col = index `mod` size
  in (row, col)

-- Tela principal
playGame :: Board -> Int -> Int -> IO Board
playGame board sizeTab sizeBomb = do
  putStrLn "Selecione uma das opções abaixo:"
  putStrLn "Letra da coluna seguida de número da linha para abrir posição"
  putStrLn "+ seguido de letra da coluna (maiúsculo) seguido de número da linha para marcar posição"
  putStrLn "- seguido de letra da coluna (maiúsculo) seguido de número da linha para desmarcar posição"
  entry <- getLine
  let action = head entry -- primeiro caractere deve ser + ou -
      params = tail entry -- restante da string
  if action == '+'
    then do
      let col = ord (head params) - ord 'A' + 1
          row = read (tail params) :: Int
      if validPosition row col sizeTab
        then do
          if limitsApp board sizeBomb
            then do
              putStrLn $ "Marcando posição " ++ [chr (row + ord 'A' - 1)] ++ show col
              updatedBoard <- markPosition board row col sizeTab sizeBomb
              printBoard updatedBoard
              playGame updatedBoard sizeTab sizeBomb
          else do
            putStrLn "O tabuleiro atingiu o máximo de posições marcadas. Desmarque uma posição antes de marcar outra!"
            playGame board sizeTab sizeBomb
      else do
        putStrLn "Posição inválida"
        playGame board sizeTab sizeBomb
  else if action == '-'
    then do
      let row = ord (head params) - ord 'A' + 1
          col = read (tail params) :: Int
      if validPosition row col sizeTab
        then do
          putStrLn $ "Desmarcando posição " ++ [chr (row + ord 'A' - 1)] ++ show col
          updatedBoard <- unmarkPosition board row col
          printBoard updatedBoard
          playGame updatedBoard sizeTab sizeBomb
        else do
          putStrLn "Posição inválida"
          return board
  else do
    let row = ord action - ord 'A' + 1
        col = read params :: Int
    if validPosition row col sizeTab
      then do
        putStrLn $ "Abrindo posição " ++ [chr (row + ord 'A' - 1)] ++ show col
        updatedBoard <- openPosition board col row sizeTab sizeBomb
        return updatedBoard
      else do
        putStrLn "Posição inválida"
        playGame board sizeTab sizeBomb


-- FUNÇÕES QUE ENVOLVEM O CAMPO MIN. (MARCA POS, DESMARCA POS, ABRE POS, TESTA VITÓRIA, TESTA DERROTA, TESTA JOGADA E SEUS AUXILIARES)

-- Testa se posição informada é válida
validPosition :: Int -> Int -> Int -> Bool
validPosition row col sizeTab = row >= 1 && row <= sizeTab && col >= 1 && col <= sizeTab

-- Função para marcar uma posição do campo
markPosition :: Board -> Int -> Int -> Int -> Int -> IO Board
markPosition board row col sizeTab sizeBomb = do
  let (currentChar, _) = board !! (row - 1) !! (col - 1)
  if currentChar == '*' || currentChar == '+'
    then do
      let updatedBoard = markCell board row col
      if checkVictory updatedBoard then 
        do
          printBoard updatedBoard
          putStrLn "PARABÉNS! VOCÊ VENCEU!"
          playGame updatedBoard sizeTab sizeBomb
        else do
          putStrLn $ "Marcando posição " ++ [chr (row + ord 'A' - 1)] ++ show col
          printBoard updatedBoard
          playGame updatedBoard sizeTab sizeBomb
    else do
      putStrLn "Posição já marcada/aberta, escolha outra"
      playGame board sizeTab sizeBomb

markCell :: Board -> Int -> Int -> Board
markCell board row col =
  let (upperRows, currentRow:lowerRows) = splitAt (row - 1) board
      updatedRow = updateList (col - 1) ('M', False) currentRow
   in upperRows ++ (updatedRow : lowerRows)

unmarkPosition :: Board -> Int -> Int -> IO Board
unmarkPosition board row col = do
  let (currentChar, _) = board !! (row - 1) !! (col - 1)
  if currentChar == 'M'
    then do
      let updatedBoard = unmarkCell board row col
      return updatedBoard
    else do
      putStrLn "Posição não marcada, escolha outra"
      return board

unmarkCell :: Board -> Int -> Int -> Board
unmarkCell board row col =
  let (upperRows, currentRow:lowerRows) = splitAt (row - 1) board
      updatedRow = updateList (col - 1) ('*', False) currentRow
  in upperRows ++ (updatedRow : lowerRows)


-- Função para verificar se todas as bombas do tabuleiro foram marcadas
checkVictory :: Board -> Bool
checkVictory board = all (\row -> all (\(c, bomb) -> not bomb || c == 'M') row) board

-- Função para limitar número de posições a serem marcadas no tabuleiro
limitsApp :: Board -> Int -> Bool
limitsApp board sizeTab = countOccurrences board 'M' <= sizeTab - 1 
  where
    countOccurrences :: Board -> Char -> Int
    countOccurrences board char = length $ filter (== char) $ concat $ map (map fst) board

-- Função para abrir uma posição do campo
openPosition :: Board -> Int -> Int -> Int -> Int -> IO Board
openPosition board row col sizeTab sizeBomb = do
  let (currentChar, isBomb) = board !! (row - 1) !! (col - 1)
  if currentChar == '*' || currentChar == '+'
    then do
      if isBomb
        then do
          putStrLn "BOOM! Você perdeu!"
          let updatedBoard = updateBoardDigit (row - 1) (col - 1) 'B' board
          revealBoard updatedBoard
          return board
        else do
          let numBombs = countAdjacentBombs board (row - 1) (col - 1) sizeTab
          let updatedBoard = updateBoardDigit (row - 1) (col - 1) (intToDigit numBombs) board
          if checkVictoryBomb updatedBoard sizeBomb then do
            putStrLn "Parabéns! Você venceu!"
            revealBoard updatedBoard
            return updatedBoard
          else do
            printBoard updatedBoard
            playGame updatedBoard sizeTab sizeBomb
    else do
      putStrLn "Posição já aberta/marcada, escolha outra"
      playGame board sizeTab sizeBomb

countAdjacentBombs :: Board -> Int -> Int -> Int -> Int
countAdjacentBombs board row col sizeTab =
  let positions = [(row - 1, col), (row + 1, col), (row, col - 1), (row, col + 1)]
      validPositions = filter (isValidPosition sizeTab) positions
      adjacentCells = map (\(r, c) -> board !! r !! c) validPositions
   in length $ filter isBomb adjacentCells


isValidPosition :: Int -> (Int, Int) -> Bool
isValidPosition sizeTab (row, col) =
  row >= 0 && row < sizeTab && col >= 0 && col < sizeTab

isBomb :: Cell -> Bool
isBomb (_, bomb) = bomb

checkVictoryBomb :: Board -> Int -> Bool
checkVictoryBomb board sizeBomb = countMarkedCells board == sizeBomb

countMarkedCells :: Board -> Int
countMarkedCells board = length $ filter (\(c, _) -> c == 'M') $ concat board

updateBoardDigit :: Int -> Int -> Char -> Board -> Board
updateBoardDigit row col newVal board =
  let (upperRows, currentRow:lowerRows) = splitAt row board
      updatedRow = updateList col (newVal, False) currentRow
  in upperRows ++ (updatedRow : lowerRows)

revealBoard :: Board -> IO ()
revealBoard board = do
  printBoard board
