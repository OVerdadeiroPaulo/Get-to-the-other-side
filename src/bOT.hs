module Testes2 where
import LI12223 
import Tarefa3_2022li1g088 

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
--vaicontraint :: (Terreno, [Obstaculo]) -> Jogador  -> Int
--vaicontraint (Estrada 0, _) = 0
--vaicontraint (Estrada vel,[]) (Jogador (x,y)) = vel
--vaicontraint par@(Estrada vel,o:bs) (Jogador (x,y))
-- |veobslinhaCoord par (x,y) == Carro = 0
-- |vel< 0 = (1+ vaicontraint (Estrada (( vel)+1),drop (x)(o:bs)++o:bs) (Jogador (x,y))) 
-- |vel> 0 =1+  vaicontraint (Estrada ( vel-1),reverse $ (o:bs) ++ take (x+1)(o:bs)) (Jogador (x,y))
--vaicontra :: (Terreno, [Obstaculo]) -> Jogador  -> Bool
--vaicontra (Estrada vel,[]) (Jogador (x,y)) = False
--vaicontra par@(Estrada vel,o:bs) (Jogador (x,y))
-- |veobslinhaCoord par (x,y) == Carro = True
-- |vel == 0 = False
-- |vel< 0 = vaicontra (Estrada ( vel+1),drop (x)(o:bs)++o:bs) (Jogador (x,y))
-- |vel> 0 = vaicontra (Estrada ( vel-1),reverse $ (o:bs) ++ take (x+1)(o:bs)) (Jogador (x,y))

--vaicontra :: (Terreno, [Obstaculo]) -> Jogador  -> Bool
--vaicontra (Estrada vel,[]) (Jogador (x,y)) = False
--vaicontra par@(Estrada vel,o:bs) (Jogador (x,y))
-- | (pos == Carro && vel > 0)|| (neg== Carro && vel < 0)= True
-- | vel == 0 = False
-- | vel > 0 = vecarro (va) (vel)
-- | vel < 0 = vecarro (iva) (  (vel))
--   where pos:va = reverse $ (o:bs) ++ take (x+1)(o:bs)
--         neg:iva = drop (x)(o:bs++o:bs)
--vecarro :: (Eq t, Num t) => [Obstaculo] -> t -> Bool
--vecarro [] _ = False
--vecarro _ 0 = False
--vecarro  (x:xs) v
--  | x == Carro = True
--  | otherwise = vecarro xs (v-1)
--veint :: [Obstaculo] -> Int -> Int
--veint [] vel = vel
--veint _ 0 = 1
--veint  (x:xs) v
--  | x == Carro = 0
--  | x/= Carro && v > 0  = 1+ veint xs (v-1) 
--  | x /= Carro && v < 0 = 1+ veint xs (v+1)
-- 
--vaicontraint :: (Terreno, [Obstaculo]) -> Jogador  -> Int
--vaicontraint (Estrada vel,[]) (Jogador (x,y)) = vel
--vaicontraint par@(Estrada vel,o:bs) (Jogador (x,y))
-- | (pos == Carro && vel > 0)= 0 
-- | (neg== Carro && vel < 0)= 0
-- | vel == 0 = 1
-- | vel > 0 = veint (va) (vel)
-- | vel < 0 = sinal vel *(veint (iva) (  (vel)))
--   where pos:va = reverse $ (o:bs) ++ take (x+1)(o:bs)
--         neg:iva = drop (x)(o:bs++o:bs)
--
--