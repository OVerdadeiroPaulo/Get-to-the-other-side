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
import Data.Type.Equality (TestEquality(testEquality))
import GHC.Real (underflowError)
{-|Funcao principal que anima o jogo usando todas as outras como auxiliare-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (a,b)) mapa@(Mapa l (((terr, (x:xs)):ys)))) jogada = Jogo (casotronco (deslocajogador(Jogador (a,b)) jogada mapa) mapa)  (daavolta (Mapa l ((terr, (x:xs)):ys)))

{-|funcao que nos diz a posicao para a qual o jogador se desloca s-}
posicaoapos :: Jogador -> Jogada -> Mapa -> Coordenadas
posicaoapos (Jogador coords) jogada mapa@(Mapa l (((terr, obs):xs)))
  | jogada == Move Cima = (fst coords, snd coords-1)
  | jogada == Move Baixo = (fst coords, snd coords+1)
  | jogada == Move Esquerda = (fst coords -1, snd coords)
  | jogada == Move Direita = (fst coords +1, snd coords)
  | jogada == Parado = coords
{-| funcao para o movimento do jogador ja com os casos em que o movimento é impossivel-}  
deslocajogador :: Jogador -> Jogada -> Mapa -> Jogador 
deslocajogador (Jogador coords) jogada mapa@(Mapa l (((terr, obs):xs)))
  | veobstaculonacoordenada mapa (ordena) == Arvore = Jogador coords
  | jogada == Parado = Jogador coords
  | (fst coords >= l && jogada == Move Direita) || (fst coords <= 0 &&  jogada == Move Esquerda) = Jogador coords
  | jogada == Move Cima && snd coords == 0 = Jogador coords
  | jogada == Move Cima = Jogador (fst coords, (snd coords-1))
  | jogada == Move Baixo = Jogador (fst coords, (snd coords+1))
  | jogada == Move Esquerda = Jogador ((fst coords -1), (snd coords))
  | jogada == Move Direita = Jogador  ((fst coords +1), (snd coords))
      where ( ordena) = (posicaoapos (Jogador coords) jogada mapa)


{-|funcao que anima as coordenadas apos a inserçao de uma linha, nao é para ser usada-}
animacoords :: Coordenadas -> Coordenadas
animacoords coords = (fst coords, (snd coords+1))
{-|funcao  auxiliar que ve o tipo de obstaculo numa dita coordenada de um terreno-}

veobstaculonalinha :: (Terreno,[Obstaculo]) ->Coordenadas -> Obstaculo
veobstaculonalinha (terr,[s]) (a,b) = s
veobstaculonalinha (terr,(x:xs))  (a,b)
  |a == 0 = x 
  |a > 0 = veobstaculonalinha (terr,xs) (a-1,b)
  |a< 0 = undefined
{-|funcao  que ve o tipo de obstaculo numa dita coordenada de um  Mapa usada para animar o mapa ou para ver onde estao as arvores, será tambem usada na tarefa 4-}

veobstaculonacoordenada :: Mapa -> Coordenadas -> Obstaculo
veobstaculonacoordenada (Mapa l ([f])) (a,b) = veobstaculonalinha f (a,b)
veobstaculonacoordenada (Mapa l (((terr, obs):xs))) (a,b)
  | b == 0 = veobstaculonalinha (terr,obs) (a,b)
  | b > 0 = veobstaculonacoordenada (Mapa l ((xs)))  (a,b-1)
  | b < 0 = undefined
{-|funcao que determina o comportamento de um jogador em cima de um tronco -}
casotronco :: Jogador -> Mapa -> Jogador
casotronco (Jogador cords) mapa@(Mapa l (((Rio vel, obs):xs)))
  | veobstaculonacoordenada mapa cords == Tronco = Jogador (fst cords  + vel, snd cords )
  | otherwise = Jogador cords
casotronco jog map = jog


{-|funcao auxiliar para daavolta  que roda so uma linha de obstaculos-}
gira :: Int -> [a] -> [a]
gira n [] = []
gira n l@(x:xs) 
  |n >= 0 = drop (length l - n) l ++ take (length l -n) l
  | n < 0 = drop (abs n) l ++ take (abs n ) l
{-| segunda funcao auxiliar para daavolta que usa pattern matching para separar o comportameto de Relva Estrada e Rio -}

giratodos :: (Terreno, [Obstaculo]) -> (Terreno , [Obstaculo])
giratodos (Rio vel, (x:xs)) = (Rio vel ,gira vel (x:xs)) 
giratodos (Estrada vel, (x:xs)) = (Estrada vel ,gira vel (x:xs)) 
giratodos (Relva, (x:xs)) = (Relva, (x:xs))
 {-|funcao que da a volta ao mapa usanso giratodos e gira como auxiliares-}
daavolta :: Mapa -> Mapa
daavolta (Mapa l (((terr, (x:xs)):ys))) =  (Mapa l ((giratodos(terr, (x:xs)): map giratodos ys)))
mapaRioTronco = Mapa 3 [(Rio  (-1), [Nenhum,Tronco,Nenhum])]
mapaarvore = Mapa 3 ([(Relva, [Nenhum,Arvore,Nenhum]),(Relva, [Arvore,Nenhum,Arvore]),(Relva, [Nenhum,Arvore,Nenhum])])
mapaunitario = Mapa 1 [(Estrada  2, [Nenhum])]
mapanormal = Mapa 2 [(Estrada  2, [Nenhum,Nenhum,Carro]),(Estrada  2, [Nenhum,Nenhum,Carro])]
jogoImpossivelMoverArvore = (Jogo (Jogador (1,1)) mapaarvore) 
jogoImpossivelLimitesMapa  = (Jogo (Jogador (0,0)) mapaunitario) 
jogoTronco = (Jogo (Jogador (1,0)) mapaRioTronco) 
jogoNormal = (Jogo (Jogador (0,1)) mapanormal) 

