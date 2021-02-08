module Types where


data State = State
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState = PacState
    {
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode

    } deriving Eq

data GhoState= GhoState
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives)
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)


instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


coloredString :: String -> Color -> String
coloredString x y
    | True = x
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x


placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

printAllModes :: [Player] -> String
printAllModes [] = ""
printAllModes ((Ghost (GhoState (n,c,v,o,p,l) m)) : ps) = show m ++ " " ++ printAllModes ps
printAllModes (((Pacman (PacState (n,c,v,o,p,l) t b m ))): ps) = show m ++ " " ++ printAllModes ps

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x

getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

getPlayerMouth :: Player -> Mouth
getPlayerMouth (Pacman (PacState (n,c,v,o,p,l) t b m )) = b

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )

getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs)
  | a == 0 = replaceNElem b p x : xs
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = []
replaceNElem i el (x:xs)
  |  i == 0 = el : xs
  | otherwise =  x : replaceNElem (i-1) el xs

getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

data Manager = Manager
    {
         state :: State
    ,    pid    :: Int
    ,    step :: Int
    ,    before :: Integer
    ,    delta :: Integer
    ,    delay :: Integer
    }


-- * Funções/Tipos alteradas por nós

-- Tipo usado na tarefa 5 para distinguir os dois modos dos fantasmas
data Mode = Catch | Run

-- * Auxiliares para a lista de jogadores

-- Encontra um jogador na lista pelo seu ID
getPlayer :: Int -> [Player] -> Player
getPlayer x (p:ps)
    | x == getPlayerID p = p
    | otherwise = getPlayer x ps
getPlayer x [] = error ("Player not found." ++ show x) 

-- Remove um jogador da lista usando o ID para localizar
remPlayer :: Int -> [Player] -> [Player]
remPlayer x [] = [] 
remPlayer x (p:ps)
    | x == getPlayerID p = ps 
    | otherwise = p:remPlayer x ps

-- Encontra o pacman na lista
getPacman :: [Player] -> Player
getPacman (p@(Pacman _): ps) = p
getPacman (_:ps) = getPacman ps
getPacman [] = error "List of players has no pacman"

{-
getPacman :: [Player] -> Player
getPacman [] = error "Pacman not found."
getPacman ((Pacman pst) : ps) = Pacman pst
getPacman ((Ghost gst) : ps) = Ghost gst
-}

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d
getPacmanMode (Ghost (GhoState ps m)) = error "This is not a Pacman."


-- * Auxiliares para posições/orientações

-- | Dá as coordenadas para as quais é suposto o jogador avançar
coords :: Orientation -- ^ Orientação que indica para que lado o jogador quer
       -> Player -- ^ Jogador
       -> Coords -- ^ Coordenadas da posição para onde o jogador vai avançar
coords o (Pacman PacState {pacState = (_,(y,x),_,_,_,_)}) =
  case o of
    L -> (y,x-1)
    R -> (y,x+1)
    U -> (y+1,x)
    D -> (y-1,x)
    Null -> (y,x)
coords o (Ghost GhoState {ghostState = (_,(y,x),_,_,_,_)}) =
  case o of
    L -> (y,x-1)
    R -> (y,x+1)
    U -> (y+1,x)
    D -> (y-1,x)
    Null -> (y,x)

-- | Move o jogador para a posição seguinte
poSeg :: Player -- ^ Jogador
      -> Player -- ^ Jogador depois de ter andado uma posição
poSeg (Pacman ps@PacState {pacState = (n,_,v,o,p,l)})
    = Pacman ps {pacState = (n,ncp,v,o,p,l)}
        where ncp = coords o (Pacman ps)
poSeg (Ghost gst@GhoState {ghostState = (n,_,v,o,p,l)})
    = Ghost gst {ghostState = (n,ncg,v,o,p,l)}
        where ncg = coords o (Ghost gst)

-- | Identifica o tipo de peça onde o jogador se encontra
actualPiece :: Player -- ^ Jogador
            -> Maze -- ^ Labirinto
            -> Piece -- ^ Peça onde o jogador se encontra
actualPiece (Pacman PacState {pacState = (_,(y,x),_,_,_,_)}) m = (m !! y) !! x
actualPiece (Ghost GhoState {ghostState = (_,(y,x),_,_,_,_)}) m = (m !! y) !! x


-- | Testa se o jogador já está com a mesma orientação pedida pela jogada ou não
testOrient :: Orientation -- ^ Orientação da jogada
           -> Player -- ^ Jogador
           -> Bool -- ^ Falso se o jogador puder avançar
testOrient o (Pacman PacState {pacState = (_,_,_,op,_,_)})
    | o == op = False
    | otherwise = True
testOrient o (Ghost GhoState {ghostState = (_,_,_,op,_,_)})
    | o == op = False
    | otherwise = True

-- | Muda a orientação de um jogador
mudaOrient :: Orientation -- ^ Orientação da jogada
           -> Player -- ^ Jogador
           -> Player -- ^ Jogador depois de mudar a orientação
mudaOrient o (Pacman ps@PacState {pacState = (n,c,v,_,p,l)})
    = Pacman ps {pacState = (n,c,v,o,p,l)}
mudaOrient o (Ghost gst@GhoState {ghostState = (n,c,v,_,p,l)})
    = Ghost gst {ghostState = (n,c,v,o,p,l)}

-- | Devolve a orientação oposta à inserida
opposite ::  Orientation -> Orientation
opposite o = case o of L -> R
                       R -> L
                       U -> D
                       D -> U
