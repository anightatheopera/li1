module Tarefa1 where

import System.Random
import Types


-- | testesT1
--
-- Testes da tarefa 1
testesT1 :: [(Int, Int, Int)]
testesT1 = [(29,37,838), (22,21,97), (30,30,98), (37,31,98)]

-- | GHOST HOUSES MODELS
--
ghostHouseUneven :: Maze
ghostHouseUneven = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                  [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty],
                  [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty],
                  [Empty, Wall, Wall, Wall, Wall, Wall, Wall,  Wall, Wall, Wall, Empty],
                  [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]

ghostHouseEven :: Maze
ghostHouseEven = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
                    [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty],
                    [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty],
                    [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty],
                    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]




-- | Gera o labirinto final
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y s = firstCorridor ++ putGhostHouse (middleKill mazeIn (middleFinder mazeIn)) ++ lastCorridor
                where  firstCorridor = lastCorridor
                       lastCorridor = firstLast x
                       rest = convertMaze(generateCont x y s)
                       mazeIn = addWall rest


-- | Gera o conteúdo do interior do labirinto
generateCont :: Int -- ^ Número de peças por corredor
             -> Int -- ^ Número de corredores
             -> Int -- ^ Seed
             -> [[Int]] -- ^ Matriz de inteiros que serão convertidos em peças
generateCont p c seed = subLists (p-2) $ generateRandoms nums seed
                       where nums = c*p-(2*c+2*p-4) -- nº de aleatórios a serem gerados(nº de peças/sem os limites)
-- prop> generateCont 8 6 612 == [[8,80,35,90,25,36],[12,74,58,7,27,12],[11,93,20,86,23,43],[66,77,17,68,42,41]]


-- | Põe um tupple em labirinto
tuppleM :: Maze -- ^ Labirinto
        -> (Int, Int) -- ^ Posição no labirinto
        -> Maze -- ^ Labirinto Resultante
tuppleM m (x,y)
     | x == 0 = m!!y:[]
     | otherwise = [m!!x,m!!y]





-- * Funções que validam um labirinto


-- | Verifica as laterais de um labirinto são válidas
validBorder :: Maze -- ^ Labirinto
            -> Bool -- ^ Laterais válidas ou não
validBorder m =
     all (==Wall) (head m) &&
     all (==Wall) (last m) &&
     all validCorridorW (middleKill m (middleFinder m)) &&
     validMiddle (tuppleM m (middleFinder m))



-- | Verifica se o túnel é válido
validTunnel :: Corridor -- ^ Corredor
            -> Bool -- ^ Corredor válido ou não válido
validTunnel [] = error "Trying to check validity of emtpy corridor"
validTunnel c = head c == Empty && last c == Empty
-- prop> validTunnel [Empty,Food Little,Empty,Food Little,Empty] == True
-- prop> validTunnel [Wall,Food Little,Empty,Food Little,Empty] == False
-- prop> validTunnel [Wall,Food Little,Empty,Food Little,Wall] == False


-- | Verifica se um corredor é válido não sendo nem a primeira nem a última lista do labirinto
validCorridorW :: Corridor -- ^ Corredor a testar
               -> Bool -- ^ Valida ou não o corredor
validCorridorW [] = True
validCorridorW (h:t) = h == Wall && last t == Wall
-- prop> validCorridorW [Wall,Empty,Empty,Food Little, Empty,Empty,Wall] == True
-- prop> validCorridorW [Empty,Empty,Food Little, Empty,Empty,Wall] == False
-- prop> validCorridorW [Wall,Empty,Empty,Food Little, Empty,Empty] == False


-- | Verifica se o meio é válido do labirinto é válido
validMiddle :: Maze -- ^ Labirinto
            -> Bool -- ^ Meio válido ou não válido
validMiddle [] = True
validMiddle (h:t) = validTunnel h && validMiddle t


-- | Verifica a validade de um labirinto inteiro
validMaze :: Maze -- ^ Labirinto
          -> Bool -- ^ Labirinto Válido ou não válido
validMaze m =
     validBorder m
     && validMiddle m



-- * Auxiliares para a casa dos fantasmas


-- | Diz qual o corredor ou corredores do meio para identificar o espaço onde fica a casa dos fantasmas
-- | Se o labirinto tiver altura par são 2 corredores, se for ímpar, apenas o segundo elemento do par é preenchido
middleFinder :: Maze -- ^ Labirinto
             -> (Int, Int) -- ^ Par de corredores que serão os túneis
middleFinder m | even l = ((div l 2), (div l 2)+1)
               | otherwise = (0, (div l 2))
               where l = length m


-- | Indica a posição da casa dos fantasmas em linha
posGHLine :: Maze -- ^ Labirinto
          -> Int -- ^ Corredor onde começará a casa dos fantasmas
posGHLine m
  | even ml = middleY - 4
  | otherwise = middleY - 2
     where (_,middleY) = middleFinder m
           ml = length m


-- | Indica a posição da casa dos fantasmas em coluna
posGHColumn :: Maze -- ^ Labirinto
            -> Int -- ^ Indica o índex dos corredores onde começará a casa dos fantasmas
posGHColumn [] = error "Trying to find coords of empty maze"
posGHColumn (h:_)
     | even x = (div (x - 10) 2)
     | otherwise = (div (x - 11) 2)
        where x = length h


-- | Tendo um labirinto original, posiciona a respetiva casa de fantasmas
putGhostHouse :: Maze -- ^ Labirinto original
              -> Maze -- ^ Labirinto já com a casa dos fantasmas
putGhostHouse [] = error "Impossible to put a ghosthouse in empty maze"
putGhostHouse m@(h:_)
     | even x = matrixInMatrix m (posGHColumn m, posGHLine m) ghostHouseEven
     | otherwise = matrixInMatrix m (posGHColumn m, posGHLine m) ghostHouseUneven
          where x = length h



-- * Funções que constroem o labirinto


-- | Gera o primeiro e último corredores do labirinto(só walls)
firstLast :: Int -- ^ Número de peças por corredor
          -> Maze -- ^ Labirinto com o primeiro e último corredores
firstLast p = [replicate p Wall]
-- prop> firstLast 6 = [[#,#,#,#,#,#]]


-- | Adiciona a primeira e última paredes nos corredores(exceto o primeiro e último) do labirinto
addWall :: Maze -- ^ Labirinto
        -> Maze -- ^ Labirinto com as paredes exteriores
addWall [] = []
addWall (h:t) = ([Wall] ++ h ++ [Wall]) : addWall t



-- | Update do index num labirinto
addTunnelCorridor :: Int -- ^ Index
                  -> Maze -- ^ Labirinto
                  -> Maze -- ^ Labirinto Resultante
addTunnelCorridor _ [] = error "Impossible to update index of empty maze"
addTunnelCorridor 0 (h:t) = ([Empty] ++ (drop 1 (take ((length h)-1) h)) ++ [Empty]) : t
addTunnelCorridor i (h:t) = h: addTunnelCorridor (i-1) t


-- | Remove o corredor do meio das listas de corredores do meio (lista de corredores excluindo a primeira e ultima lista)
middleKill :: Maze -- ^ Labirinto
           -> (Int, Int)
           -> Maze
middleKill [] _ = []
middleKill m (0, b) = addTunnelCorridor b m
middleKill m (x,y) = addTunnelCorridor (x-1) (addTunnelCorridor (y-1) m)



-- * Auxiliares para matrizes e corredores


-- | Pega numa lista e substitui uma segunda lista pelos elementos da lista anterior no índice desejado
listInList :: [a] -- ^ Primeira Lista
           -> Int -- ^ Índice
           -> [a] -- ^ Segunda Lista
           -> [a] -- ^ Lista Resultante
listInList (h:t) i (x:xs)
     | i == 0 = x : listInList t i xs
     | otherwise = h : listInList t (i - 1) (x:xs)
listInList l _ [] = l
listInList _ _ _ = []
-- prop> listInList [1,2,3,4] 2 [1,2] == [1,2,1,2]


-- | Pega numa matriz e numa posição da mesma, e coloca uma lista na posiçao dada
listInMatrix :: [[a]] -- ^ Matriz
             -> (Int, Int) -- ^ Posição da Matriz
             -> [a] -- ^ Lista para substituir
             -> [[a]] -- ^ Matriz Resultante
listInMatrix [] _ _ = error "Empty matrix"
listInMatrix (h:t) (x,y) l
     | y == 0 = listInList h x l : t
     | otherwise = h : listInMatrix t (x,y-1) l
-- prop> listInMatrix listInMatrix [[1,2,3],[4,5,6]] (0,1) [7,8,9] == [[1,2,3],[7,8,9]]


-- | Insere uma matriz numa certa posição de outra matriz
matrixInMatrix :: [[a]] -- ^ Primeira Matriz
               -> (Int, Int) -- ^ Posição da primeira matriz
               -> [[a]] -- ^ Segunda Matriz
               -> [[a]] -- ^ Matriz Resultante
matrixInMatrix _ _ [] = error "Second matrix is empty"
matrixInMatrix m (x,y) (h:[]) = listInMatrix m (x,y) h
matrixInMatrix m (x,y) (h:t) = matrixInMatrix (listInMatrix m (x,y) h) (x,y+1) t
-- prop> matrixInMatrix




-- * Funções do Generator

-- | Given a seed returns a list of n integer randomly generated
--
generateRandoms :: Int -> Int -> [Int]
generateRandoms n seed = let gen = mkStdGen seed -- creates a random generator
                         in take n $ randomRs (0,99) gen

-- Converts list into a list of list of size n
--
subLists :: Int -> [a] -> [[a]]
subLists _ [] = []
subLists p l = take p l : subLists p (drop p l)

-- | Converts an integer number into a Peca
-- 3 <=> Comida Grande
-- 0 <= n < 7 <=> Comida Pequena
-- 7 < n <= 9 <=> Parede
--
convertPiece :: Int -> Piece
convertPiece n
               | n == 3 = Food Big
               | n >= 0 && n < 70 = Food Little
               | n >= 70 && n <= 99 = Wall
convertPiece _ = error "Invalid Number"


-- | Converts a list of integers into a Corredor
--
convertCorridor :: [Int] -> Corridor
convertCorridor [] = []
convertCorridor (h:t) = convertPiece h : convertCorridor t

-- | Converts a list of lists of integers into a Labirinto
--
convertMaze :: [[Int]] -> Maze
convertMaze [] = []
convertMaze (c:z) = convertCorridor c : convertMaze z
