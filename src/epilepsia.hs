{- |
Module      : Tarefa3_2022li1g088
Description : Movimentação do personagem e obstáculos
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g088 where

import LI12223
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
type Imagens = [Picture]
type State = (Int,(Float,Float))
redSquare :: Picture
redSquare =  color red (Polygon [(-1000,-1000),(1000,-1000),(1000,1000),(-1000,1000),(-1000,-1000)])

blueTriangle :: Picture
blueTriangle = color blue (Polygon [(-1000,-1000),(-1000,1000),(1000,0),(-1000,-1000)])

greenCircle :: Picture
greenCircle = color green (ThickCircle 0 2000)

yellowRectangle :: Picture
yellowRectangle = color yellow (Polygon [(-1250,-750),(1250,-750),(1250,750),(-1250,750),(-1250,-750)])


drawState :: State -> Picture
drawState (0,(x,y)) = translate x y redSquare
drawState (1,(x,y)) = translate x y blueTriangle
drawState (2,(x,y)) = translate x y greenCircle
drawState (3,(x,y)) = translate x y yellowRectangle

eventChange :: Event -> State -> State
eventChange (EventKey (SpecialKey KeyRight) Down _ _) (n,(x,y)) = (n,(x+20,y))
eventChange (EventKey (SpecialKey KeyLeft) Down _ _) (n,(x,y)) = (n,(x-20,y))
eventChange (EventKey (SpecialKey KeyUp) Down _ _) (n,(x,y)) = (n,(x,y+20))
eventChange (EventKey (SpecialKey KeyDown) Down _ _) (n,(x,y)) = (n,(x,y-20))
eventChange _ s = s

timeChange :: Float -> State -> State
timeChange f (3,p) = (0,p)
timeChange f (n,p) = (n+1,p)


linePic :: Picture
linePic = Line [((-1000),(-1000)),((-1000),1000),(1000,1000),(1000,(-1000)),((-1000),(-1000))]


displayMode :: Display
displayMode = InWindow "Game" (640,640) (0,0)


main :: IO()
main = do play displayMode white 40 (0,(0,0)) drawState eventChange timeChange
