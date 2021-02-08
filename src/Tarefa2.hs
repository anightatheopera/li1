module Tarefa2 where
    
import Types
import FileUtils

defaultDelayTime = 250

-- | Função principal:
-- | Testa se o jogador não está morto, com a orientação errada ou a entrar num túnel
-- | Caso nenhuma destas situações ocorra, o estado será alterado pela função updateState
play :: Play -> State -> State
play (Move x o) (State m ps l)
    | x == getPlayerID (getPacman ps) = updateStatePac (unMega $ mouthMovement $ megaTime $ getPacman ps) o (State m ps l)
    | otherwise = updateStateGhost (backToNormal (getPacman ps) (getPlayer x ps)) o (State m ps l)

-- | Função que altera o estado do jogo quando é feita uma jogada com um pacman
updateStatePac :: Player -- ^ Jogador
               -> Orientation -- ^ Orientação do Jogador
               -> State -- ^ Estado atual do jogo
               -> State -- ^ Estado atualizado
updateStatePac p o (State m ps l)
    | pacMode == Dying = State m ps l
    | testOrient o p = State m (mudaOrient o p:remPlayer pacID ps) l
    | nextPiece o actualCoords m == Wall = State m ps l
    | checkLeft p m = State m (telepLeft p m: remPlayer pacID ps) l
    | checkRight p m = State m (telepRight p m: remPlayer pacID ps) l
    | piece == Food Little || piece == Food Big = State cleanMaze updatedList l
    | piece == Empty = State m updatedList l
    | otherwise = State m ps l
         where actualCoords = getPlayerCoords p
               nextCoords = getPlayerCoords movedPlayer
               piece = nextPiece o actualCoords m
               cleanMaze = replaceElemInMaze nextCoords Empty m
               pacID = getPlayerID p
               pacMode = getPacmanMode movedPlayer
               movedPlayer = poSeg p
               ghosts = howManyGhosts nextCoords (ghostCoords ps) 0
               newPac = eat movedPlayer piece pacMode ghosts
               updatedList = newPac : remPlayer pacID (updateGhosts nextCoords pacMode ps m)
            

-- | Função que altera o estado do jogo quando é feita uma jogada com um ghost
updateStateGhost :: Player
                 -> Orientation
                 -> State
                 -> State
updateStateGhost p o (State m ps l)
    | testOrient o p = State m (mudaOrient o p:nps) l
    | nextPiece o cs m == Wall = State m ps l
    | checkLeft p m = State m (telepLeft p m:nps) l
    | checkRight p m = State m (telepRight p m:nps) l
    | cs == cps && pacMode == Mega = State m (eatGhost p m:nps) l
    | otherwise = State m ps l
        where iD = getPlayerID p
              cs = getPlayerCoords p
              cps = getPlayerCoords $ getPacman ps
              nps = remPlayer iD ps
              pacMode = getPacmanMode $ getPacman ps

mouthMovement :: Player -> Player
mouthMovement (Pacman pacState) =
                let oldMouth = openClosed pacState
                    newMouth = case oldMouth of 
                                    Open -> Closed
                                    Closed -> Open
                in Pacman pacState{openClosed = newMouth}

-- | Função que diminui o tempo mega quando o Pacman está em modo mega
megaTime :: Player -- ^ Jogador
            -> Player -- ^ Jogador com o tempo atualizado
megaTime (Pacman pacState@(PacState{timeMega = t})) = Pacman pacState{timeMega = max 0 (t - fromIntegral defaultDelayTime)} 
megaTime ghost = ghost

-- | Função que muda o modo do Pacman para normal quando o tempo mega acaba
unMega :: Player -- ^ Jogador
        -> Player -- ^ Jogador após o término do tempo mega
unMega (Pacman pacState@(PacState{pacmanMode = Mega}))
    | timeMega pacState <= 0 = Pacman pacState{pacmanMode = Normal}
unMega player = player

-- | Função que muda o modo dos Fantasmas quando o Pacman volta ao modo normal
backToNormal :: Player -- ^ Pacman
               -> Player -- ^ Fantasma
               -> Player -- ^ Fantasma com o modo atualizado
backToNormal (Pacman PacState{pacmanMode = Mega}) (Ghost ghostState) = Ghost ghostState{ghostMode = Dead}
backToNormal (Pacman PacState{pacmanMode = _}) (Ghost ghostState) = Ghost ghostState{ghostMode = Alive}


eat :: Player -> Piece -> PacMode -> Int -> Player
eat (Pacman ps@PacState {pacState = (n,c,v,o,p,l)}) (Food Little) Normal x
    | x /= 0 && l == 0
        = Pacman ps{pacState = (n,c,v,o,p+1,l), pacmanMode = Dying}
    | x /= 0 && l > 0
        = Pacman ps{pacState = (n,c,v,o,p+1,l-1)}
    | x == 0
        = Pacman ps{pacState = (n,c,v,o,p+1,l)}
eat (Pacman ps@PacState {pacState = (n,c,v,o,p,l)}) (Food Little) Mega x
    | x /= 0
        = Pacman ps{pacState = (n,c,v,o,p+1+x,l)}
    | x == 0
        = Pacman ps{pacState = (n,c,v,o,p+1,l)}
eat (Pacman ps@PacState {pacState = (n,c,v,o,p,l)}) (Food Big) Normal x
    | x /= 0 && l == 0
        = Pacman ps{pacState = (n,c,v,o,p+5,l), pacmanMode = Dying}
    | x /= 0 && l > 0
        = Pacman ps{pacState = (n,c,v,o,p+5,l-1)}
    | x == 0
        = Pacman ps{pacState = (n,c,v,o,p+5,l), pacmanMode = Mega}
eat (Pacman ps@PacState {pacState = (n,c,v,o,p,l)}) (Food Big) Mega x
    | x /= 0
        = Pacman ps{pacState = (n,c,v,o,p+5+x,l)}
    | x == 0
        = Pacman ps{pacState = (n,c,v,o,p+5,l)}
eat (Pacman ps@PacState {pacState = (n,c,v,o,p,l)}) Empty Normal x
    | x /= 0 && l == 0
        = Pacman ps{pacState = (n,c,v,o,p,l), pacmanMode = Dying}
    | x /= 0 && l > 0
        = Pacman ps{pacState = (n,c,v,o,p,l-1)}
    | x == 0
        = Pacman ps{pacState = (n,c,v,o,p,l)}
eat (Pacman ps@PacState {pacState = (n,c,v,o,p,l)}) Empty Mega x
    | x /= 0
        = Pacman ps{pacState = (n,c,v,o,p+x,l)}
    | otherwise
        = Pacman ps{pacState = (n,c,v,o,p,l)}
eat p _ _ _ = p

updateGhosts :: Coords -> PacMode -> [Player] -> Maze -> [Player]
updateGhosts _ _ [] _ = []
updateGhosts cs Mega (Ghost gst@GhoState{ghostState = (n,c,v,o,p,l)}:gs) m
    | c == cs
        = Ghost gst{ghostState = (n,nc,v,o,p,l), ghostMode = Alive} : updateGhosts cs Mega gs m
    | otherwise
        = Ghost gst : updateGhosts cs Mega gs m
            where nc = (div (length(head m)) 2, div (length m) 2)
updateGhosts _ _ (Ghost gst:gs) _ = Ghost gst:gs
updateGhosts _ _ (Pacman pst:ps) _ = Pacman pst:ps


ghostCoords :: [Player] -> [Coords]
ghostCoords [] = []
ghostCoords ((Ghost GhoState {ghostState = (_,c,_,_,_,_)}:ps)) = c:ghostCoords ps
ghostCoords (_:ps) = ghostCoords ps

howManyGhosts :: Coords -> [Coords] -> Int -> Int
howManyGhosts _ [] _ = 0
howManyGhosts c (p:ps) i
    | p == c = howManyGhosts c ps i+1
    | otherwise = howManyGhosts c ps i

eatGhost :: Player -> Maze -> Player
eatGhost (Pacman pst) _ = Pacman pst
eatGhost (Ghost gst@GhoState {ghostState = (n,_,v,o,p,l)}) m
    = Ghost gst {ghostState = (n,nc,v,o,p,l), ghostMode = Alive}
        where nc = (div (length(head m)) 2, div (length m) 2)

-- * Auxiliares da condição do túnel

-- | Encontra a posição do(s) túnel(eis) da esquerda
findLeftTunnel :: Maze -- ^ Labirinto
               -> [Coords] -- ^ Coordenadas do(s) tunel(eis) da esquerda
findLeftTunnel m
    | even (length m) = [(div l 2-1, 0), (div l 2, 0)]
    | otherwise = [(div l 2, 0)]
        where l = length m


-- | Encontra a posição do(s) túnel(eis) da direita
findRightTunnel :: Maze -- ^ Labirinto
                -> [Coords] -- ^ Coordenadas do(s) tunel(eis) da direita
findRightTunnel m
    | even (length m) = [(div l 2-1,length(head m)-1),(div l 2,length(head m)-1)]
    | otherwise = [(div l 2,length(head m)-1)]
        where l = length m


-- | Testa se o jogador se move para o túnel da esquerda
checkLeft :: Player -- ^ Jogador
          -> Maze -- ^ Labirinto
          -> Bool -- Se true, move-se para a esquerda, se False, não
checkLeft p m = (getPlayerOrientation p == L) && elem (getPlayerCoords p) (findLeftTunnel m)

-- | Testa se o jogador se move para o túnel da direita
checkRight :: Player -- ^ Jogador
           -> Maze -- ^ Labirinto
           -> Bool -- Se true, move-se para a direita, se False, não
checkRight p m = (getPlayerOrientation p == R) && elem (getPlayerCoords p)(findRightTunnel m)


-- | Teletransporta o jogador para o túnel da esquerda
telepLeft :: Player -- ^ Jogador
          -> Maze -- ^ Labirinto
          -> Player -- ^ Jogador depois de ter sido teletransportado para o lado esquerdo do tunel
telepLeft (Pacman ps@PacState {pacState = (n,(y,_),v,o,p,l)}) m =
    Pacman ps{pacState = (n,(y,length(head m)-1),v,o,p,l)}
telepLeft (Ghost gst@GhoState {ghostState = (n,(y,_),v,o,p,l)}) m =
    Ghost gst{ghostState = (n,(y,length(head m)-1),v,o,p,l)}


-- | Teletransporta o jogador para o túnel da direita
telepRight :: Player -- ^ Jogador
           -> Maze -- ^ Labirinto
           -> Player -- ^ Jogador depois de ter sido teletransportado para o lado direito do tunel
telepRight (Pacman ps@PacState {pacState = (n,(y,_),v,o,p,l)}) _
    = Pacman ps{pacState = (n,(y,0),v,o,p,l)}
telepRight (Ghost gst@GhoState {ghostState = (n,(y,_),v,o,p,l)}) _
    = Ghost gst{ghostState = (n,(y,0),v,o,p,l)}

-- | Identifica o tipo de peça para o qual o jogador quer avançar
nextPiece :: Orientation -- ^ Orientação do jogador
          -> Coords -- ^ Coordenadas do Jogador
          -> Maze -- ^ Labirinto
          -> Piece -- ^ Peça para onde o jogador irá avançar
nextPiece o (y,x) m
    | o == L && elem (y,x) (findLeftTunnel m) == False = (m !! y)!!(x-1)
    | o == R && elem (y,x) (findRightTunnel m) == False = (m !! y)!!(x+1)
    | o == U = (m !! (y+1)) !! x
    | o == D = (m !! (y-1)) !! x
    | o == Null = (m !! y) !! x
    | otherwise = Empty
