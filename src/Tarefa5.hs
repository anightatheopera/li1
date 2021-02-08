{- |
Nesta tarefa, pretendia-se implementar uma jogada automática para cada um dos fantasmas.

Para este efeito, na função principal, testamos a lista de jogadores para 3 casos
1. Se for pacman, nada acontece
2. Se for um ghost que está vivo, deve devolver uma jogada calculada pela função chaseMode.
3. Se for um ghost que está morto deve devolver uma jogada calculada pela função scatterMode.

Primeiro, criamos uma função chamada idealCoords, que tanto para a chaseMode como para a scatterMode vai calcular as coordenadas mais
apropriadas para onde o fantasma se deve deslocar em relação ao pacman.(se alguma das coordenadas indicar uma wall, será removida da lista)

Para ajudar nesta tarefa, criamos o tipo "Mode" que pode ser "Catch" ou "Run"(catch para o chase mode e run para o scatter mode)
A idealCoords recebe a orientação do ghost, o modo em que ele está(Catch ou Run), o labirinto, as coordenadas do Pacman, e a lista de coordenadas
adjacentes às do Ghost(calculadas pela função surround), ou seja, as posições para onde é possível este avançar no momento.
Faz alguns cálculos de distância entre a posição do pacman e as posições para onde o ghost se pode mover, escolhendo a mais próxima ou a mais afastada,
tendo em conta se o ghost está a perseguir ou a fugir.

Após tudo isto, a função goTo recebe a posição atual do ghost e a posição escolhida para este se movimentar, transformando isto na orientação que este deve seguir.

A função chooseOrient reune estas funções, devolvendo quer à chaseMode como à scatterMode a orientação escolhida.

-}

module Tarefa5 where
import Types
import Tarefa2 ( nextPiece )
import FileUtils


-- | Função principal
ghostPlay :: State -- ^ Estado do jogo
          -> [Play] -- ^ Lista de jogadas para cada fantasma
ghostPlay (State _ [] _) = []
ghostPlay (State _ [Pacman _] _) = []
ghostPlay (State m (p@(Pacman _) :ps) l) = ghostPlay (State m (ps ++ [p]) l)
ghostPlay (State m (Ghost gst :ps) l)
    | ghostMode gst == Alive
        = chaseMode (State m (Ghost gst :ps) l) iD : ghostPlay (State m ps l)
    | otherwise
        = scatterMode (State m (Ghost gst :ps) l) iD : ghostPlay (State m ps l)
        where iD = getPlayerID (Ghost gst)


-- | Função que indica uma jogada em que o ghost persegue o pacman
chaseMode :: State -- ^ Estado do jogo
          -> Int -- ^ ID do ghost que vai efetuar a jogada
          -> Play -- ^ Jogada a efetuar
chaseMode (State m ps _) iD = Move iD (chooseOrient pacCoords ghostCoords pacOrient m Catch)
        where ghostCoords = getPlayerCoords $ getPlayer iD ps
              pacCoords = getPlayerCoords $ getPacman ps
              pacOrient = getPlayerOrientation $ getPacman ps

-- | Função que indica uma jogada em que o ghost foge do pacman
scatterMode :: State -- ^ Estado do jogo
            -> Int -- ^ ID do ghost que vai efetuar a jogada
            -> Play -- ^ Jogada a efetuar
scatterMode (State m ps _) iD = Move iD (chooseOrient pacCoords ghostCoords ghostOrient m Run)
                     where ghostCoords = getPlayerCoords $ getPlayer iD ps
                           pacCoords = getPlayerCoords $ getPacman ps
                           ghostOrient = getPlayerOrientation $ getPlayer iD ps

-- | Função que dadas as coordenadas do pacman e do ghost que joga, escolhe a melhor orientação
-- para o último tomar na sua jogada usando as funções a baixo
chooseOrient :: Coords -- ^ Coordenadas do Pacman
             -> Coords -- ^ Coordenadas do Ghost
             -> Orientation -- ^ Orientação do Ghost
             -> Maze -- ^ Labirinto
             -> Mode -- ^ Modo do ghost(fugir/perseguir)
             -> Orientation -- ^ Orientação que deve ser escolhida
chooseOrient (yp,xp) (yg,xg) o m Catch =
    goTo (yg,xg) (idealCoords o Catch m (yp,xp) (surround (yg,xg)))
chooseOrient (yp,xp) (yg,xg) o m Run =
    goTo (yg,xg) (idealCoords o Run m (yp,xp) (surround (yg,xg)))


-- | Função que calcula em que direção o fantasma se encontra mais próximo ou afastado do pacman
-- dependendo se quer persegui-lo, ou fugir dele. Para essa diferenciação, temos o tipo Mode
-- que pode ser Catch ou Run
idealCoords :: Orientation -- ^ Orientação do ghost
            -> Mode -- ^ Modo do ghost(fugir/perseguir)
            -> Maze -- ^ Labirinto
            -> Coords -- ^ Coordenadas do Pacman
            -> [Coords] -- ^ Lista de coordenadas adjacentes às da posição do ghost
            -> Coords -- ^ Coordenadas para onde o ghost se deve dirigir
idealCoords _ _ _ _ [] = error "Coords not found"
idealCoords _ _ _ _ [(y,x)] = (y,x)
idealCoords o Catch m (yp,xp) ((yg1,xg1):(yg2,xg2):ps)
    | (m !! yg1) !! xg1 == Wall = idealCoords o Run m (yp,xp) ((yg2,xg2):ps)
    | (((yp-yg1)^2) + ((xp-xg1)^2)) <= (((yp-yg2)^2) + ((xp-xg2)^2))
        = idealCoords o Catch m (yp,xp) ((yg1,xg1):ps)
    | otherwise = idealCoords o Catch m (yp,xp) ((yg2,xg2):ps)

idealCoords o Run m (yp,xp) ((yg1,xg1):(yg2,xg2):ps)
    | (m !! yg1) !! xg1 == Wall = idealCoords o Run m (yp,xp) ((yg2,xg2):ps)
    | (((yp-yg1)^2) + ((xp-xg1)^2)) >= (((yp-yg2)^2) + ((xp-xg2)^2))
        = idealCoords o Run m (yp,xp) ((yg1,xg1):ps)
    | otherwise = idealCoords o Run m (yp,xp) ((yg2,xg2):ps)

-- | Função que converte as coordenadas pretendidas na orientação que deve ser tomada para lá chegar
goTo :: Coords -- ^ Coordenadas originais
     -> Coords -- ^ Coordenadas para onde se pretende ir
     -> Orientation -- ^ Orientação que deve ser escolhida
goTo (y1,x1) (y2,x2)
    | y1 < y2 = U
    | y1 > y2 = D
    | x1 > x2 = L
    | x1 < x2 = R
goTo _ _ = error "Not valid coords"


-- | Função que dadas umas coordenadas devolve a lista de todas as coordenadas adjacentes
-- para qualquer orientação
surround :: Coords -- ^ Coordenadas originais
         -> [Coords] -- ^ Lista de coordenadas adjacentes
surround (y,x) = [(y,x-1),(y,x+1),(y+1,x),(y-1,x)]
