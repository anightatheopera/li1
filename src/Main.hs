module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa4
import Tarefa5
import Tarefa6


loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime )

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer k m = m{state = s{playersState = nps}}
  where
    o = case k of
      KeyUpArrow    -> D
      KeyDownArrow  -> U
      KeyLeftArrow  -> L
      KeyRightArrow -> R
    s = state m
    pl = playersState s
    p = getPacman pl
    np = mudaOrient o p
    nps = updatePacman np pl

updatePacman :: Player -> [Player] -> [Player]
updatePacman _ [] = []
updatePacman p ((Pacman _):t) = (p:t)
updatePacman p (h:t) = h:updatePacman p t

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0
                      drawString $ show (state man)
                    render

currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager state p step b delta delay) = Manager state p step now (delta+(now-b)) delay

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager state p step b d delay) = (Manager state p step now 0 delay)

nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager state p step b d delay) = Manager (passTime step state) p (step+1) now 0 delay


loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager
