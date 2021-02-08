module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5
import FileUtils

{- |
O objetivo da tarefa 4 é reagir à passagem de tempo. No entanto, grande parte da execução da mesma passou por adicionar certas 
funções à tarefa 2 de modo a atualizar a função principal da mesma, a função play.
Para além disso, tínhamos que completar o ficheiro Main.hs, que faz o jogo ganhar vida
utilizando a biblioteca NCurses. Conseguimos por o ficheiro Main a correr mas com bastantes
dificuldades, uma vez que a biblioteca NCurses é bastante antiga e não havia maneira de a instalar
na nossa distribuição de Linux (Manjaro). Tivemos, assim, que instalar uma virtual machine com Ubuntu.
/Objetivos:/ 
    - __Fazer o Pacman abrir e fechar a boca alternadamente entre cada jogada;__
      Para este efeito foi criada a função mouthMovement que abria ou fechada a boca tendo em conta o estado da boca anteriormente.

    - __Fazer que quando o Pacman entre em modo Mega perca tempo mega em cada jogada;__
      Dado um tempo mega t, a função megaTime remove o defaultDelayTime a cada iteração da jogada.

    - __Fazer com que o Pacman volte ao modo Alive se o tempo mega for <=0 e estiver em Mega;__
      Tendo em conta o tempo mega do Pacman, a função unMega muda o modo do Pacman para Normal se esse tempo for igual ou menor que 0.

    - __Fazer com que os Fantasmas voltem ao modo Alive se não houver nenhum Pacman em modo Mega;__
      Sabendo o modo do Pacman através do seu estado, a função backToNormal altera o modo do Fantasma para Alive.

    - __Fazer com que os jogadores devem progridam N jogadas em cada iteração da função, em que N é um número inteiro inferido a partir da velocidade do jogador e paridade da iteração;__
      Utilizando a função flooredCeiling para saber quantas vezes vai ser necessário chamar a função play e a função playAll para executar essas jogadas, faz-se com que os jogadores progridam N jogadas em cada iteração.

Além destas funções principais tivemos que criar funções auxiliares que permitem com que o código corra fluídamente sendo estas:
    - __whichPlay__, que traduz a jogada do Pacman quando recebe um estado;
   
    - __mergePlays__, que junta as jogadas dadas pela função whichPlay numa lista.

Por fim, ao juntar as funções referidas anteriormente, conseguimos implementar a função principal passTime, que dado umstep e um estado devolve um novo estado que foi alterado pela passagem do tempo.
O objetivo desta tarefa, é, essencialmente, atualizar a função play da tarefa 2 de modo a que esta suporte jogadas de fantasmas e fazer com que os estados sejam alterados consoante o tempo passado.
-}


defaultDelayTime = 250 -- 250 ms

-- | Função Principal
passTime :: Int -- ^ Número do step
        -> State -- ^ Estado do Jogo
        -> State -- ^ Estado após a reação do tempo passado
passTime x s = playAll (mergePlays s) s x

-- | Função que calcula o número de vezes a executar a função play
flooredCeiling :: Double -- ^ Velocidade
                -> Int -- ^ Número do Step
                -> Int -- ^ Número de vezes a executar a função
flooredCeiling vel s 
    | mod s 2 == 0 = floor vel
    | otherwise = ceiling vel

-- | Função que dado o estado do jogo diz qual a próxima jogada a efetuar
whichPlay :: State -- ^ Estado do Jogo
            -> Play -- ^ Jogada Final 
whichPlay s = Move a b
    where
        a = getPlayerID $ getPacman $ playersState s
        b = getPlayerOrientation $ getPacman $ playersState s

-- | Função que executa as jogadas dos fantasmas e do Pacman
playAll :: [Play] -- ^ Conjunto de jogadas 
          -> State -- ^ Estado do jogo
          -> Int -- ^ Step
          -> State -- ^ Estado Final
playAll [] s _ = s
playAll (p@(Move x _):ps) s step = playAll ps ns step
    where
        (_, _, v, _, _, _) = getPlayerState $ getPlayer x (playersState s)
        n = flooredCeiling v step
        ns = timesPlay n p s

-- | Função que repete N vezes uma certa jogada
timesPlay :: Int -- ^ Número de vezes a repetir
            -> Play -- ^ Jogada
            -> State -- ^ Estado
            -> State -- ^ Estado Final
timesPlay n p state 
    | n == 0 = state 
    | otherwise = timesPlay (n-1) p (play p state)


-- | Função que dado um estado junta as jogadas numa lista
mergePlays :: State -- ^ Estado do jogo
             -> [Play] -- ^ Lista de jogadas
mergePlays s = (whichPlay s) : ghostPlay s