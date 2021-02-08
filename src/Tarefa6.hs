{- |

Para esta tarefa, dividimos a função principal em duas, sendo ghost a função que gera uma jogada automática para um ghost, e pacman que gera uma jogada
automática para o pacman.

No caso "ghost" usamos as funções já definidas na tarefa5

No caso "pacman" foram verificadas 3 casos e criadas 2 novas funções: a eatFood e a closeGhost

- O primeiro caso consiste em o pacman estar em estado Mega, nesse estado, este consegue comer os fantasmas e, por isso, neste caso usamos a função da tarefa 5
para perseguir o fantasma mais próximo(usando duas vezes a idealCoords conseguimos primeiro o fantasma mais perto, e depois as coordenadas disponiveis para o pacman
mais próximas desse fantasma)

- O segundo caso é o pacman estar em estado Normal, aí vemos se alguma das peças adjacentes é uma comida com a função "eatFood"(dando prioridade às Food Big
pois dão mais pontuação e colocam o pacman em estado Mega) e se não tem nenhum fantasma na direção dessa mesma comida(verificado com a "closeGhost"). Nesse caso,
o pacman avança, caso contrário, passa para o terceiro caso.

- O terceiro caso é o pacman estar em modo normal, sem comida por perto, ou com algum fantasma por perto. Aqui, voltamos a implementar a idealCoords no modo
primeiro no modo Catch para identificar o fantasma mais próximo, e depois em modo Run pra escolher as coordenadas mais longe do mesmo

-}
module Tarefa6 where

import Types
    ( getPacman,
      getPlayerID,
      Coords,
      FoodType(Big, Little),
      GhoState(ghostMode),
      GhostMode(Alive),
      Maze,
      Mode(Catch,Run),
      Orientation(..),
      PacMode(Normal, Mega),
      PacState(PacState, pacState, pacmanMode),
      Piece(Food),
      Play(..),
      Player(..),
      State(State) )
import Tarefa2(ghostCoords)
import Tarefa5 ( chaseMode, scatterMode, idealCoords, goTo, surround )

-- | Função Principal
bot :: Int -- ^ ID do Jogador
    -> State -- ^ Estado do Jogo
    -> Maybe Play -- ^ Jogada a efetuar
bot iD (State m ps l)
    | iD == getPlayerID (getPacman ps)
        = pacman iD (getPacman ps) (State m ps l)
    | otherwise
        = ghost iD (State m ps l)

-- | Função que indica uma jogada para um fantasma
-- | Usa as funções implementadas na tarefa 5
ghost :: Int -- ^ ID do Jogador
      -> State -- ^ Estado do Jogo
      -> Maybe Play -- ^ Jogada a efetuar
ghost iD (State m (Ghost gst:gs) l)
    | ghostMode gst == Alive = Just (chaseMode (State m (Ghost gst:gs) l) iD)
    | otherwise = Just (scatterMode (State m (Ghost gst:gs) l) iD)
ghost _ _ = Nothing


-- | Função que indica uma jogada para um pacman
-- Se o pacman estiver em modo mega, persegue o fantasma mais próximo, se estiver em modo normal
-- e tiver comida na peça seguinte, sem fantasmas por perto, avança para comê-la, caso contrário,
-- foge do(s)  possível(eis) fantasma(s)
pacman :: Int -- ^ ID do Pacman
       -> Player -- ^ Pacman
       -> State -- ^ Estado do Jogo
       -> Maybe Play -- ^ Jogada a efetuar
pacman x (Pacman PacState{pacState = (_,c,_,o,_,_), pacmanMode = Mega}) (State m ps _)
    = Just (Move x (goTo c whereTo))
        where gs = ghostCoords ps
              whereTo = idealCoords o Catch m (idealCoords o Catch m c gs) (surround c)
pacman x (Pacman PacState{pacState = (_,c,_,o,_,_), pacmanMode = Normal}) (State m ps _)
    | eatFood c gs m /= Null = Just (Move x (eatFood c gs m))
    | otherwise = Just (Move x (goTo c whereTo))
        where gs = ghostCoords ps
              whereTo = idealCoords o Run m (idealCoords o Catch m c gs) (surround c)

pacman _ _ _ = Nothing

-- | Função que indica a orientação que o pacman deve tomar tendo em conta as peças de comida,
-- dando prioridade às Food Big, e verificando a presença de fantasmas
eatFood :: Coords -- ^ Coordenadas do Pacman
        -> [Coords] -- ^ Lista com as coordenadas de todos os fantasmas
        -> Maze -- ^ Labirinto
        -> Orientation -- ^ Orientação usada na jogada a efetuar
eatFood (y,x) gs m
    |upPiece == Food Big && closeGhost U (y,x) gs == 0 = U
    |downPiece == Food Big && closeGhost D (y,x) gs == 0 = D
    |leftPiece == Food Big && closeGhost L (y,x) gs == 0 = L
    |rightPiece == Food Big && closeGhost R (y,x) gs == 0 = R
    |upPiece == Food Little && closeGhost U (y,x) gs == 0 = U
    |downPiece == Food Little && closeGhost D (y,x) gs == 0 = D
    |leftPiece == Food Little && closeGhost L (y,x) gs == 0 = L
    |rightPiece == Food Little && closeGhost R (y,x) gs == 0 = R
        where upPiece = (m !! y) !! (x+1)
              downPiece = (m !! y) !! (x-1)
              leftPiece = (m !! (y-1)) !! x
              rightPiece = (m !! (y+1)) !! x
eatFood _ [] _ = Null
eatFood _ _ _ = Null


-- | Função que recebe a lista de coordenadas de todos os fantasmas e testa se existe algum fantasma
-- a menos de 2 blocos do pacman, na orientação indicada
closeGhost :: Orientation -- ^ Orientação na qual devemos procurar fantasmas
           -> Coords -- ^ Coordenadas do Pacman
           -> [Coords] -- ^ Lista de coordenadas de todos os Fantasmas
           -> Int -- ^ Inteiro que vai funcionar como verdadeiro ou falso
closeGhost _ _ [] = 0
closeGhost Null _ _ = 1
closeGhost U (yp,xp) ((yg,_):ps)
    | yg <= yp + 2 = 1
    | otherwise = closeGhost U (yp,xp) ps
closeGhost D (yp,xp) ((yg,_):ps)
    | yp <= yg - 2 = 1
    | otherwise = closeGhost D (yp,xp) ps
closeGhost L (yp,xp) ((_,xg):ps)
    | xg <= xp + 2 = 1
    | otherwise = closeGhost L (yp,xp) ps
closeGhost R (yp,xp) ((_,xg):ps)
    | xp <= xg - 2 = 1
    | otherwise = closeGhost R (yp,xp) ps
