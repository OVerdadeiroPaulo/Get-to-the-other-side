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

{-|funcao que nos diz a posicao para a qual o jogador iria sem obstaculos-}
deslocaria :: Jogador -> Jogada -> Coordenadas
deslocaria (Jogador coords) jogada 
  | jogada == Parado = coords
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
veobstaculonacoordenada (Mapa l (((terr, []):ys))) (a,b) = Nenhum
veobstaculonacoordenada (Mapa l ([])) (a,b) = Nenhum
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
--cirobs :: Mapa -> Coordenadas -> Coordenadas
{-nao sei se esta bem-}

deslocaobs :: Mapa -> Coordenadas -> Obstaculo
deslocaobs mapa@(Mapa l (((Rio vel, (h:t)):xs))) (a,b)
  |a == 0 = veobstaculonacoordenada (mapa) (l,b)
  | a == l = veobstaculonacoordenada mapa (0,b)
  |otherwise = veobstaculonacoordenada mapa (a+vel,b)
deslocaobs mapa@(Mapa l (((Estrada vel, (h:t)):xs))) (a,b)
  |a == 0 = veobstaculonacoordenada (mapa) (l,b)
  | a == l = veobstaculonacoordenada mapa (0,b)
  |otherwise = veobstaculonacoordenada mapa (a+vel,b)
omapatest = Mapa 2 ([(Rio 2, [Tronco,Tronco]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Tronco,Tronco]),(Rio 2, [Tronco,Tronco]),(Estrada 2, [Nenhum,Carro])])
