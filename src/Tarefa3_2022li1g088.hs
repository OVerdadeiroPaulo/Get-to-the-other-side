{- |
Module      : Tarefa3_2022li1g088
Description : Movimentação do personagem e obstáculos
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g088 where

import LI12223

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo = undefined

{-|funcao que nos diz a posicao para a qual o jogador se desloca-}
deslocajogador :: Jogador -> Jogada -> Mapa -> Coordenadas
deslocajogador (Jogador coords) jogada mapa@(Mapa l ([(terr, obs)]))
  | veobstaculonacoordenada mapa (deslocajogador (Jogador coords) jogada (Mapa l ([(terr, obs)]))) == Arvore = coords
  | jogada == Parado = coords
  | fst coords > l || fst coords < 0 = coords
  | jogada == Move Cima && snd coords == 0 = coords
  | jogada == Move Cima = (fst coords, (snd coords-1))
  | jogada == Move Baixo = (fst coords, (snd coords+1))
  | jogada == Move Esquerda = ((fst coords -1), (snd coords))
  | jogada == Move Direita = ((fst coords +1), (snd coords))
{-|funcao que anima as coordenadas apos a inserçao de uma linha-}
animacoords :: Coordenadas -> Coordenadas
animacoords coords = (fst coords, (snd coords+1))
{-|funao que ve o tipo de obstaculo numa dita coordenada-}
veobstaculonacoordenada :: Mapa -> Coordenadas -> Obstaculo
veobstaculonacoordenada (Mapa l (((terr, [x]):ys))) (a,b) = x
veobstaculonacoordenada (Mapa l ([(x,y)])) (a,b) = last y
veobstaculonacoordenada (Mapa l (((terr, (x:xs)):ys))) (a,b) 
  | a == 0 && b == 0 = x
  | b== 0 && a /= 0 = veobstaculonacoordenada (Mapa l (((terr, (xs)):ys))) (a-1,b)
  | a == 0 && b /= 0 = veobstaculonacoordenada (Mapa l (((terr, (x:xs)):ys))) (a, b-1)
  | a /= 0 && b /= 0 = veobstaculonacoordenada (Mapa l ((ys))) (a-1, b-1)
{-|funcao que determina o comportamento de um jogador em cima de um tronco -}
casotronco :: Jogador -> Mapa -> Coordenadas
casotronco (Jogador cords) mapa@(Mapa l (((Rio vel, obs):xs)))
  | veobstaculonacoordenada mapa cords == Tronco = (fst cords  + vel, snd cords )
  | otherwise = cords

{-nao sei se esta bem-}

deslocaobs :: Mapa -> Coordenadas -> Obstaculo
deslocaobs mapa@(Mapa l (((Rio vel, (h:t)):xs))) (a,b)
  |a == 0 = veobstaculonacoordenada (mapa) (l-(vel-1),b)
  | a == l = veobstaculonacoordenada mapa (0+(vel-1),b)
  |otherwise = veobstaculonacoordenada mapa (a+vel,b)
deslocaobs mapa@(Mapa l (((Estrada vel, (h:t)):xs))) (a,b)
  |a == 0 = veobstaculonacoordenada (mapa) (l-(vel-1),b)
  | a == l = veobstaculonacoordenada mapa (0+(vel-1),b)
  |otherwise = veobstaculonacoordenada mapa (a+vel,b)
deslocaobs mapa@(Mapa l ([(Relva, obs)])) (a,b) = veobstaculonacoordenada mapa (a,b)
omapatest = Mapa 2 ([(Rio 2, [Tronco,Tronco]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Nenhum,Carro,Tronco]),(Rio 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Tronco,Tronco]),(Rio 2, [Nenhum,Carro]),(Estrada 2, [Nenhum,Carro])])

gira :: Int -> [a] -> [a]
gira n [] = []
gira n l@(x:xs) 
  |n >= 0 = drop n l ++ take (length l -n) l
  | n < 0 = undefined
--daavolta (Mapa l (((terr, (x:xs)):ys)))