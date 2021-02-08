module Tarefa3 where

import Data.List
import Types
import Tarefa1


testMazeEnunc =
    [ replicate 5 Wall
    , [ Wall, Food Little, Food Little, Food Big, Wall ]
    , [ Wall, Food Little, Food Little, Food Little, Wall ]
    , [ Wall, Food Little, Food Little, Food Little, Wall ]
    , [ Wall, Food Little, Food Big, Food Little, Wall]
    , replicate 5 Wall
    ]


-- | testesT3
--
-- Testes da tarefa 3
testesT3 :: [Instructions]
testesT3 = [compactMaze (generateMaze 29 37 838), compactMaze (generateMaze 22 21 97), compactMaze (generateMaze 30 30 98), compactMaze (generateMaze 37 31 98)]


-- | compactMaze
--
-- Função principal
compactMaze :: Maze -> Instructions
compactMaze m = compactMazeAux [] m


-- * Padrões Horizontais

-- | horizontalP
--
-- Utilizando a patternH constroi uma Instruction que devolve a lista de instruções de um corredor
horizontal :: Corridor -- ^ Corredor
           -> Instruction -- ^ Instruções
horizontal (h:t) = Instruct $ patternH (1,h) t
-- prop> horizontal [Wall,Empty,Empty,Wall,Empty,Food Little,Empty,Wall] == Instruct [(1,#),(2, ),(1,#),(1, ),(1,.),(1, ),(1,#)]


-- | patternH
--
-- Auxiliar para verificar os padrões
-- Devolve uma lista de instruções compactadas(junta as que são repetidas sucessivamente)
patternH :: (Int, Piece) -- ^ Primeiro par que vai ser comparado aos restantes da lista
         -> Corridor -- ^ Corredor
         -> [(Int, Piece)] -- ^ Lista de pares(repetidos estão compactados)
patternH x [] = [x]
patternH (x,p) (h:t)
    | p == h = patternH ((x+1), p) t
    | otherwise = (x,p) : patternH (1,h) t
-- prop> horizontal [Wall,Empty,Empty,Wall,Empty,Food Little,Empty,Wall] == Instruct [(1,#),(2, ),(1,#),(1, ),(1,.),(1, ),(1,#)]


-- * Padrões Verticais

-- | compactMazeAux
--
-- Compacta as instruções eliminando as repetições de corredores iguais sucessivos
compactMazeAux :: Instructions -- ^ Instruções do primeiro corredor
               -> Maze -- ^ Labirinto
               -> Instructions -- ^ Instruções finais
compactMazeAux acc [] = acc
compactMazeAux acc (c:cs) =
    let i = horizontal c
    in case findIndex (==i) acc of
        Just index -> compactMazeAux (acc ++ [Repeat index]) cs
        Nothing    -> compactMazeAux (acc ++ [i]) cs
-- prop> compactMaze [[Wall,Empty,Food Little,Wall],[Wall,Empty,Food Little,Wall],[Wall,Empty,Empty,Wall]] == [Instruct [(1,#),(1, ),(1,.),(1,#)],Repeat 0,Instruct [(1,#),(2, ),(1,#)]]
