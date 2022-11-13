{- |
Module      : Tarefa2_2022li1g088
Description : Geração contínua de um mapa
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g088 where

import LI12223 
import Tarefa1_2022li1g088
import System.Random
import LI12223 (Mapa, Terreno)
import Tarefa1_2022li1g088 (inicionovo)
import Tarefa1_2022li1g088 (tipodeobs)
import Data.List (elemIndex)
import Data.String (String)


{-|Funcao estendemapa, que adiciona uma linha ao mapa -}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((te,obs):xs)) a = Mapa l ((te2,obs2):(te,obs):xs)
                                 where te2 = ter29 a (te,obs) (ter28 a (Mapa l ((te,obs):xs))) 
                                       obs2 = obs28 l (te2, []) a
                           

{-|Funcao que escolhe um Terreno aleatorio-}                                  
ter28:: Int -> Mapa -> Terreno
ter28 a te' | aleatoriofinal a == 1 = head (proximosTerrenosValidos te')  
            | aleatoriofinal a == 2 = last (proximosTerrenosValidos te') 
            | otherwise = head (tail (proximosTerrenosValidos te'))   


{-|Funcao que adiciona velocidade aos Terrenos e certifica que temos sempre rios opostos-}
ter29 :: Int -> (Terreno,[Obstaculo]) -> Terreno -> Terreno 
ter29 a (Rio vel1,obs) (Rio vel) | vel1 > 0 = Rio (aleatorio4'final a) 
                                 | otherwise = Rio (aleatorio4''final a)      
ter29 a (te,obs) (Rio vel) = Rio (aleatorio4final a)   
ter29 a (te,obs) (Estrada vel) = Estrada (aleatorio4final a)
ter29 a (te,obs) Relva = Relva     
         

{-|Funcao responsavel por selecionar a lista de obstaculos-}
obs28 :: Int -> (Terreno,[Obstaculo]) -> Int -> [Obstaculo]
obs28 l (te2, b) a | l == (length b) = b
                   | otherwise = let obst = proximosObstaculosValidos l (te2, b)
                                     obs3 = obs3' obst a 
                                  in obs28 l (te2, b ++ [obs3] ) a 

obs3' :: [Obstaculo] -> Int -> Obstaculo
obs3' b a | aleatoriofinal' a == 1 = head b
          | aleatoriofinal' a == 2 = last b
            

{-|Funcao que cria um numero aleatorio entre 0 a 100-}

listarandom :: Int -> Int -> [Int]
listarandom seed len = take len (randoms (mkStdGen seed))

--aleatoriode0a100 :: Int -> Int
--aleatoriode0a100 k =1+ abs ((head(listarandom (k) (1) )) `mod` (100))

aleatoriofinal :: Int -> Int
aleatoriofinal k= 1+ abs ((head(listarandom (k) (1) )) `mod` (3))

aleatoriofinal' :: Int -> Int
aleatoriofinal' k= 1+ abs ((head(listarandom (k) (1) )) `mod` (2))

so4 :: Int -> Int
so4 k = 1+ abs ((head(listarandom (k) (1) )) `mod` (8)) 

aleatorio4final :: Int -> Int
aleatorio4final k 
  | so4 k >= 5 =  1 + (mod  (so4 k)  5)
  | otherwise =  -so4 k

aleatorio4'final :: Int -> Int
aleatorio4'final k 
  | so4 k >= 5 =  -(1 + (mod  (so4 k)  5))
  | otherwise  =  -so4 k

aleatorio4''final :: Int -> Int
aleatorio4''final k 
  | so4 k >= 5 =  1 + (mod  (so4 k)  5)
  | otherwise = so4 k

{-| Funcao que verifica os proximos terrenos validos-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva] 
proximosTerrenosValidos mapa@(Mapa l ((te,obs):xs))
                                              | tipodeobs mapa && terrenoscontiguos mapa && isrioFIM mapa = [Estrada 0, Relva]
                                              | tipodeobs mapa && terrenoscontiguos mapa && isestradaFIM mapa = [Rio 0, Relva]
                                              | tipodeobs mapa && terrenoscontiguos mapa && isrelvaFIM mapa = [Estrada 0, Rio 0]
                                              | not (tipodeobs mapa) || not (terrenoscontiguos mapa ) = []
                                              | otherwise = [Rio 0, Estrada 0, Relva]

{-| Funcao que verifica se temos o numero limite de Rios-}
isrioFIM :: Mapa -> Bool
isrioFIM mapa = isRio2 1 mapa

isRio2 :: Int -> Mapa -> Bool
isRio2 _ (Mapa _ []) = False
isRio2 4 (Mapa _ ((te,obs):xs)) = True 
isRio2 n (Mapa l ((te,obs):xs)) | inicionovo te == "Rio" = isRio2 (n+1)  (Mapa l (xs))
                                | otherwise = False

{-| Funcao que verifica se temos o numero limite de estradas-}
isestradaFIM :: Mapa -> Bool
isestradaFIM mapa = isEstrada2 1 mapa

isEstrada2 :: Int -> Mapa -> Bool
isEstrada2 _ (Mapa _ []) = False
isEstrada2 5 (Mapa _ ((te,obs): xs)) = True
isEstrada2 n (Mapa l ((te,obs): xs)) | inicionovo te == "Est" = isEstrada2 (n+1) (Mapa l (xs))
                                     | otherwise = False

{-| Funcao que verifica se temos o numero limite de Relva-}                                    
isrelvaFIM :: Mapa -> Bool
isrelvaFIM mapa = isRelva2 1 mapa

isRelva2 :: Int -> Mapa -> Bool
isRelva2 _ (Mapa _ []) = False 
isRelva2 5 (Mapa _ ((te,obs):xs)) = True
isRelva2 n (Mapa l ((te,obs):xs)) | inicionovo te == "Rel" = isRelva2 (n+1) (Mapa l (xs))
                                  | otherwise = False



{-|Funcao que verifica os possiveis proximos obstaculos validos-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos _ (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos _ (Estrada vel, []) = [Nenhum,Carro]
proximosObstaculosValidos n (te, (x:xs)) | inicionovo te == "Rio" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]                        
                                         | inicionovo te == "Rio" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Tronco (x:xs)) = [Tronco]                     
                                         | inicionovo te == "Rio" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum,Tronco]                     
                                         | inicionovo te == "Rel" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]
                                         | inicionovo te == "Rel" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Arvore (x:xs)) = [Arvore] 
                                         | inicionovo te == "Rel" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum,Arvore]
                                         | inicionovo te == "Est" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]
                                         | inicionovo te == "Est" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Carro (x:xs)) = [Carro] 
                                         | inicionovo te == "Est" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum, Carro]
                                         | otherwise = []

{-| Funcao que verifica se os obstaculos correspondem ao tipo de Terreno-}
tipobs :: (Terreno,[Obstaculo]) -> Bool
tipobs (_, []) = True
tipobs (te, (x:xs))
  | inicionovo te == "Rel" && x == Arvore || x == Nenhum = tipobs (te, xs) 
  | inicionovo te == "Rio" && x == Tronco || x == Nenhum = tipobs (te, xs)
  | inicionovo te == "Est" && x == Carro  || x == Nenhum = tipobs (te, xs)
  | otherwise = False


mapatest22 = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])