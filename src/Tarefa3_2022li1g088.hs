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
import Data.ByteString (elemIndex)
import Data.Maybe (fromMaybe)
import Tarefa1_2022li1g088 (inicionovo)
{-|Funcao principal que anima o jogo usando todas as outras como auxiliare-}
animaJogo :: Jogo -> Jogada -> Jogo
--animaJogo (Jogo (Jogador (a,b)) mapa@(Mapa l (((terr, x:xs):ys)))) jogada = Jogo (casotronco (deslocajogador(Jogador (a,b)) jogada mapa) mapa)  (daavolta (Jogador (a,b)) jogada (Mapa l ((terr, x:xs):ys)))

--animaJogo (Jogo (Jogador (a,b)) mapa@(Mapa l (((terr, x:xs):ys)))) jogada = Jogo ( deslocajogador (casotronco(Jogador (a,b)) mapa) jogada mapa)  (daavolta (Jogador (a,b)) jogada (Mapa l ((terr, x:xs):ys)))

animaJogo (Jogo (Jogador (a,b)) mapa@(Mapa l (((terr, x:xs):ys)))) jogada =
  let newJogador = casotronco (deslocajogador (Jogador (a,b)) jogada mapa) mapa
  in Jogo newJogador (daavolta newJogador jogada (Mapa l ((terr, x:xs):ys)))

--animaJogo (Jogo (Jogador (a,b)) mapa@(Mapa l (((terr, x:xs):ys)))) jogada = Jogo ( (deslocafinal(Jogador (a,b)) jogada mapa) )  (daavolta ( (deslocafinal(Jogador (a,b)) jogada mapa) )  jogada (Mapa l ((terr, x:xs):ys)))


{-|funcao que nos diz a posicao para a qual o jogador se desloca s-}

posicaoapos :: Jogador -> Jogada -> Mapa -> Coordenadas
posicaoapos (Jogador coords) jogada mapa@(Mapa l (((terr, obs):xs)))
  | jogada == Move Cima = (fst coords, snd coords -1)
  | jogada == Move Baixo = (fst coords, snd coords +1)
  | jogada == Move Esquerda = (fst coords -1, snd coords)
  | jogada == Move Direita = (fst coords +1, snd coords)
  | jogada == Parado = (fst coords , snd coords)
{-| funcao para o movimento do jogador ja com os casos em que o movimento é impossivel-}  
deslocajogador :: Jogador -> Jogada -> Mapa -> Jogador 
deslocajogador (Jogador coords) jogada mapa@(Mapa l (((terr, obs):xs)))
  | jogada == Parado = (Jogador coords)
  | veobstaculonacoordenada mapa ordena == Arvore = (Jogador coords)
  | (fst coords >= l && jogada == Move Direita) || (fst coords <= 0 &&  jogada == Move Esquerda) = (Jogador coords)
  | jogada == Move Cima && snd coords == 0 = (Jogador coords)
  | jogada == Move Cima = (Jogador (fst coords, snd coords -1))
  | jogada == Move Baixo = (Jogador (fst coords, snd coords +1))
  | jogada == Move Esquerda = (Jogador (fst coords -1, snd coords))
  | jogada == Move Direita = (Jogador  (fst coords +1, snd coords))
  | otherwise = (Jogador coords)
      where  ordena = posicaoapos (Jogador coords) jogada mapa
deslocafinal jog jogada mapa = casotronco ( deslocajogador jog jogada mapa) mapa
{-|funcao  auxiliar que ve o tipo de obstaculo numa dita coordenada de um terreno-}

veobstaculonalinha :: (Terreno,[Obstaculo]) ->Coordenadas -> Obstaculo
veobstaculonalinha (terr,[s]) (a,b) = s
veobstaculonalinha (terr,x:xs)  (a,b)
  |a == 0 = x 
  |a > 0 = veobstaculonalinha (terr,xs) (a-1,b)
  |a< 0 = undefined
{-|funcao  que ve o tipo de obstaculo numa dita coordenada de um  Mapa usada para animar o mapa ou para ver onde estao as arvores, será tambem usada na tarefa 4-}

veobstaculonacoordenada :: Mapa -> Coordenadas -> Obstaculo
veobstaculonacoordenada (Mapa l [f]) (a,b) = veobstaculonalinha f (a,b)
veobstaculonacoordenada (Mapa l (((terr, obs):xs))) (a,b)
  | b == 0 = veobstaculonalinha (terr,obs) (a,b)
  | b > 0 = veobstaculonacoordenada (Mapa l xs)  (a,b-1)
  | b < 0 = undefined
{-|funcao que determina o comportamento de um jogador em cima de um tronco -}
casotronco :: Jogador -> Mapa -> Jogador
casotronco (Jogador cords) mapa@(Mapa l (((terr, obs):xs)))
  | veobstaculonacoordenada mapa cords == Tronco =  (Jogador (fst cords  + velocidade terr2, snd cords )) 
   where (Mapa l ((terr2,(o:bs)):outs)) = velinha (Jogador cords) mapa
casotronco jog mape = jog
--auxiliar para casotronco 
velinha :: Jogador -> Mapa -> Mapa
velinha (Jogador (a,b)) (Mapa l ((terr,(x:xs)):ys)) 
 | b == 0 = (Mapa l ((terr,(x:xs)):ys))
 | otherwise = velinha (Jogador (a,b-1)) (Mapa l (ys)) 


{-|funcao auxiliar para daavolta  que roda so uma linha de obstaculos-}
gira :: Int -> [a] -> [a]
gira n [] = []
gira n l@(x:xs) 
  |n > 0 = drop (length l - n) l ++ take (length l -n) l
  | n < 0 = drop (abs n) l ++ take (abs n ) l
  | n == 0 = l
{-| segunda funcao auxiliar para daavolta que usa pattern matching para separar o comportameto de Relva Estrada e Rio -}
desmapa  (Mapa l (filling)) =  filling
emmapa filling =  (Mapa 12 (filling))


daavolta ::Jogador  -> Jogada-> (Mapa)->  Mapa
daavolta jog@(Jogador (a,b)) gada mapa@(Mapa l ([])) = (Mapa l ([]))
daavolta jog@(Jogador (a,b)) gada mapa@(Mapa l lista@((par@(terr, x:xs):ys))) = 
  case terr of 
        Rio vel ->  (Mapa l (((terr ,gira vel (x:xs)): desmapa ( daavolta  (Jogador (a,b-1)) gada (emmapa ys)))))
           where k= 0 
        Relva -> (Mapa l (((terr ,(x:xs)) :desmapa ( daavolta (Jogador (a,b-1))  gada (emmapa ys)))))
           where k = 0
        Estrada vel 
            | (x:xs) !! a /= Carro || b /= 0 -> (Mapa l (((terr ,gira vel (x:xs)): desmapa ( daavolta  (Jogador (a,b-1))  gada (emmapa ys)))))
            -- gada == (Move Direita) && (veobslinhaCoord par (a+1,b) == Carro) && vel <0 -> (Mapa l (((terr ,(x:xs)) :desmapa ( daavolta (Jogador (a,b+1))  gada (emmapa ys))))) 
            -- gada == (Move Esquerda) && ( veobslinhaCoord par  (a-1,b) == Carro) && vel>=0-> (Mapa l (((terr ,(x:xs)) :desmapa ( daavolta (Jogador (a,b+1))  gada (emmapa ys))))) 
            -- (x:xs) !! a == Carro && b == 0 -> (Mapa l (((terr ,(x:xs)) :(desmapa ( daavolta  (Jogador (a,b-1))  gada (emmapa ((ys)))))))) 
            | vaicontra (terr, x:xs) (Jogador (a,b+1)) && b == 0  -> (Mapa l (((terr ,gira (vaicontraint (terr, x:xs)  jog ) (x:xs)): desmapa ( daavolta  (Jogador (a,b-1))  gada (emmapa ys)))))
            -- gada == Parado  ->  (Mapa l (((terr ,gira vel (x:xs)): desmapa ( daavolta  (Jogador (a,b+1))  gada (emmapa ys)))))
            | otherwise -> (Mapa l (((terr ,(x:xs)) :(desmapa ( daavolta  (Jogador (a,b-1))  gada (emmapa ((ys)))))))) 
             where ori:ginal = lista
                   k=0 
roda :: Jogador-> Jogada -> [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
roda _ _ [] = []
roda jog@(Jogador (a,b)) gada (li:nhas) = rodaum jog gada li : roda jog gada nhas

rodaum jog@(Jogador (a,b)) gada li
  | inicionovo ( fst li )== "Est"  &&  veobslinhaCoord li (a,b) == Carro  =  (fst li , gira (velocidade $ fst li) (snd li) )
  | inicionovo ( fst li) == "Est" &&  gada == (Move Esquerda) && ( veobslinhaCoord li  (a-1,b) == Carro) && (velocidade (fst li) )>=0 = li
  | inicionovo ( fst li) == "Est" &&  gada == (Move Direita) && ( veobslinhaCoord li  (a+1,b) == Carro) && (velocidade (fst li) )>=0 = li
  | inicionovo ( fst li) == "Est" && vaicontra (li) jog= (fst li ,gira (vaicontraint (li)  jog ) (snd li))
  | inicionovo ( fst li) == "Rio" = (fst li, gira (velocidade (fst li)) (snd li) ) 
  | inicionovo ( fst li) == "Rel" = li
  | otherwise = li
indice _ [] = -1
indice x (li:sta) 
  | x== li = 1
  | otherwise = 1+ indice x sta


-- toma em atenchao a velocidade e usa a para animajogo com vel 1 se a vel+y> y2>y 
veobslinhaCoord ::  (Terreno, [Obstaculo]) -> Coordenadas -> Obstaculo
veobslinhaCoord par@(terr,o:bs) (x,y) 
  |x== 0 = o
  | otherwise = veobslinhaCoord (terr, bs) (x-1,y)



{-vaicontra :: (Terreno, [Obstaculo]) -> Jogador  -> Bool
vaicontra (Estrada vel,[]) (Jogador (x,y)) = False
vaicontra par@(Estrada vel,o:bs) (Jogador (x,y))
 |veobslinhaCoord par (x,y) == Carro = True
 |vel == 0 = False
 |vel< 0 = vaicontra (Estrada (abs vel-1),drop (x)(o:bs)++o:bs) (Jogador (x,y))
 |vel> 0 = vaicontra (Estrada (abs vel-1),reverse $ (o:bs) ++ take (x+1)(o:bs)) (Jogador (x,y))
-}


vaicontra :: (Terreno, [Obstaculo]) -> Jogador  -> Bool
vaicontra (Estrada vel,[]) (Jogador (x,y)) = False
vaicontra par@(Estrada vel,o:bs) (Jogador (x,y))
 | pos == Carro || neg== Carro = True
 | vel == 0 = False
 | vel > 0 = vecarro (va) (vel)
 | vel < 0 = vecarro (iva) ( abs (vel))
   where pos:va = reverse $ (o:bs) ++ take (x+1)(o:bs)
         neg:iva = drop (x)(o:bs++o:bs)
vecarro :: (Eq t, Num t) => [Obstaculo] -> t -> Bool
vecarro [] _ = False
vecarro _ 0 = False
vecarro  (x:xs) v
  | x == Carro = True
  | otherwise = vecarro xs (v-1)

vaicontraint :: (Terreno, [Obstaculo]) -> Jogador  -> Int
vaicontraint (Estrada vel,[]) (Jogador (x,y)) = vel
vaicontraint par@(Estrada vel,o:bs) (Jogador (x,y))
 |veobslinhaCoord par (x,y) == Carro = 0
 |vel == 0 = 0
 |vel< 0 = sinal vel * (1+ vaicontraint (Estrada ((abs vel)-1),drop (x)(o:bs)++o:bs) (Jogador (x,y))) 
 |vel> 0 =1+  vaicontraint (Estrada (abs vel-1),reverse $ (o:bs) ++ take (x+1)(o:bs)) (Jogador (x,y)) 



velocidade (Rio vel) = vel
velocidade (Estrada vel) = vel
velocidade (Relva) = 0
sinal x 
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
mapaRioTronco = Mapa 3 [(Rio  (-1), [Nenhum,Tronco,Nenhum])]
mapaarvore = Mapa 3 ([(Relva, [Nenhum,Arvore,Nenhum]),(Relva, [Arvore,Nenhum,Arvore]),(Relva, [Nenhum,Arvore,Nenhum])])
mapaunitario = Mapa 1 [(Estrada  2, [Nenhum])]
mapanormal = Mapa 2 [(Relva, [Nenhum,Nenhum,Carro]),(Estrada  2, [Nenhum,Nenhum,Carro]),(Estrada  2, [Nenhum,Nenhum,Carro])]
jogoImpossivelMoverArvore = (Jogo (Jogador (1,1)) mapaarvore) 
jogoImpossivelLimitesMapa  = (Jogo (Jogador (0,0)) mapaunitario) 
jogoTronco = (Jogo (Jogador (1,0)) mapaRioTronco) 
jogoNormal :: Jogo
jogoNormal = (Jogo (Jogador (0,1)) mapanormal) 
jogo2= Jogo (Jogador (6,8)) (Mapa 12 [(Estrada (2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum])]) 

jogo3= Jogo (Jogador (5,8)) (Mapa 12 [(Estrada (2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum])]) 
teste1 = animaJogo jogo3 Parado
teste2 = animaJogo jogo2 Parado
teste3 = animaJogo jogo3 (Move Direita)


