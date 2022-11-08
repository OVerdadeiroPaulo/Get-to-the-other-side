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
  | jogada == Move Cima = (fst coords, (snd coords-1))
  | jogada == Move Baixo = (fst coords, (snd coords+1))
  | jogada == Move Esquerda = ((fst coords -1), (snd coords))
  | jogada == Move Direita = ((fst coords +1), (snd coords))
{-|funcao que anima as coordenadas apos a inserçao de uma linha-}
animacoords :: Coordenadas -> Coordenadas
animacoords coords = (fst coords, (snd coords-1))
{-|funao que ve o tipo de obstaculo numa dita coordenada-}
veobstaculonacoordenada :: Mapa -> Coordenadas -> Obstaculo
veobstaculonacoordenada (Mapa l (((terr, (x:xs)):ys))) (a,b) 
  | a == 0 && b == 0 = x
  | 