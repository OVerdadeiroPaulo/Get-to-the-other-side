{- |
Module      : Tarefa3_2022li1g088
Description : Movimentação do personagem e obstáculos
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g088 where

import LI12223
import Data.Data (dataTypeRep)

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (a,b)) (mapa@(Mapa l (((terr, (x:xs)):ys))))) jogada = (Jogo (Jogador (a,b)) (daavolta (Mapa l (((terr, (x:xs)):ys)))))

{-|funcao que nos diz a posicao para a qual o jogador se desloca-}
{-deslocajogador :: Jogador -> Jogada -> Mapa -> Coordenadas
deslocajogador (Jogador coords) jogada mapa@(Mapa l ([(terr, obs)]))
  | veobstaculonacoordenada mapa (deslocajogador (Jogador coords) jogada (Mapa l ([(terr, obs)]))) == Arvore = coords
  | jogada == Parado = coords
  | fst coords > l || fst coords <= 0 = coords
  | jogada == Move Cima && snd coords == 0 = coords
  | jogada == Move Cima = (fst coords, (snd coords-1))
  | jogada == Move Baixo = (fst coords, (snd coords+1))
  | jogada == Move Esquerda = ((fst coords -1), (snd coords))
  | jogada == Move Direita = ((fst coords +1), (snd coords))
-}
deslocajogador :: Jogador -> Jogada -> Mapa -> Jogador 
deslocajogador (Jogador coords) jogada mapa@(Mapa l ([(terr, obs)]))
  | veobstaculonacoordenada mapa (deslocajogador (Jogador coords) jogada (Mapa l ([(terr, obs)]))) == Arvore = Jogador coords
  | jogada == Parado = Jogador coords
  | fst coords > l || fst coords <= 0 = Jogador coords
  | jogada == Move Cima && snd coords == 0 = Jogador coords
  | jogada == Move Cima = Jogador (fst coords, (snd coords-1))
  | jogada == Move Baixo = Jogador (fst coords, (snd coords+1))
  | jogada == Move Esquerda = Jogador ((fst coords -1), (snd coords))
  | jogada == Move Direita = Jogador  ((fst coords +1), (snd coords))
    |where Jogador ordena = 


{-|funcao que anima as coordenadas apos a inserçao de uma linha, nao é para ser usada-}
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

{-nao esta bem-}

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
{-|funcao auxiliar para daavolta -}
gira :: Int -> [a] -> [a]
gira n [] = []
gira n l@(x:xs) 
  |n >= 0 = drop (length l - n) l ++ take (length l -n) l
  | n < 0 = drop (abs n) l ++ take (abs n ) l
{-| segunda funcao auxiliar para daavolta -}

giratodos :: (Terreno, [Obstaculo]) -> (Terreno , [Obstaculo])
giratodos (Rio vel, (x:xs)) = (Rio vel ,gira vel (x:xs)) 
giratodos (Estrada vel, (x:xs)) = (Estrada vel ,gira vel (x:xs)) 
giratodos (Relva, (x:xs)) = (Relva, (x:xs))
 {-|funcao que da a volta ao mapa-}
daavolta :: Mapa -> Mapa
daavolta (Mapa l (((terr, (x:xs)):ys))) =  (Mapa l ((giratodos(terr, (x:xs)): map giratodos ys)))

mapatest = Mapa 2 ([(Rio 1, [Nenhum,Nenhum ,Tronco]),(Rio (-1), [Tronco,Nenhum,Tronco]),(Estrada 1, [Nenhum,Nenhum,Carro])])
parteste = (Rio 1 ,[Tronco, Tronco, Tronco,Tronco, Nenhum , Tronco])
