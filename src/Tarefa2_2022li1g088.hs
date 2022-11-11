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
import LI12223 (Mapa)
import Tarefa1_2022li1g088 (inicio)
import Data.List (elemIndex)


{-estendeMapa :: Mapa -> Int -> Mapa 
estendeMapa (Mapa l linha) a = let  te' = proximosTerrenosValidos (Mapa l linha)
                                            te2 :: Int -> [Terreno] -> Terreno
                                            te2 a te' | mod (aleatoriode0a100 a) 2 == 0 = (head te') 
                                                      | aleatoriode0a100 a >= 50 = (last te')
                                                      | otherwise = head (tail te') 
                               in  Mapa l ((te2,obs2): linha)
-}
{-|Funcao estendemapa, que adiciona uma linha ao mapa -}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((te,obs):xs)) a = let  te' = proximosTerrenosValidos (Mapa l((te,obs):xs))                                  
                                            te2 = if mod (aleatoriode0a100 a) 2 == 0 then (head te') else if(aleatoriode0a100 a) >= 50 then (last te') else head (tail te') 
                                            obs2 = obs28 l (te2, []) a
                                        in  Mapa l ((te2,obs2):(te,obs):xs)
                             
{-|Funcao responsavel por selecionar a lista de obstaculos-}
obs28 :: Int -> (Terreno,[Obstaculo]) -> Int -> [Obstaculo]
obs28 l (te2, b) a | l == (length b) = b
                   | otherwise = let obst = proximosObstaculosValidos l (te2, b)
                                     obs3 = if mod (aleatoriode0a100 a) 2 == 0 then head obst else last obst
                                  in obs28 l (te2, b ++ [obs3] ) a 



{-obs' :: Int -> Int -> (Terreno,[Obstaculo]) -> Obstaculo
obs' a l (te2, (x:xs)) | mod (aleatoriode0a100 a) 2 == 0 = head (proximosObstaculosValidos l (te2, (x:xs)))
                       | otherwise = last (proximosObstaculosValidos l (te2, (x:xs)))-}
                      
{-}                                       
obs28 :: Int -> (Terreno,[Obstaculo]) -> Int -> [Obstaculo]
obs28 l (te2, b) a | l == (length b) = b
                   | b == [] || l > length b = obs28 l (te2, (obs' a l (te2, b)): b) a
-}
{-
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa la linhas@(x:xs)) | terreno == Rio 0 = (Mapa la ((Rio ve, obstaculos):xs)) 
                                    | terreno == Estrada 0 = (Mapa la ((Estrada ve, obstaculos):xs))
                                    | terreno == Relva = (Mapa la ((relva, obstaculos):xs))
            where obstaculos = proximosObstaculosValidos la  x 
                  terreno = proximosTerrenosValidos (Mapa la linhas @(x:xs))
                  ve = (head randomIntsL)
-}
{-|Funcao que cria um numero aleatorio entre 0 a 100-}
unlist::[a] -> a
unlist (x:xs) =  x
randomIntsL :: Int -> Int -> [Int]
randomIntsL seed len = take len (randoms (mkStdGen seed))
aleatoriode0a100 :: Int -> Int
aleatoriode0a100 k = abs ((unlist(randomIntsL (k) (1) )) `mod` (100))



{-| Funcao que verifica os proximos terrenos validos-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva] 
proximosTerrenosValidos (Mapa l ((te,obs):xs))  | isrioFIM (Mapa l ((te,obs):xs)) = [Estrada 0, Relva]
                                                | isestradaFIM (Mapa l ((te,obs):xs)) = [Rio 0, Relva]
                                                | isrelvaFIM (Mapa l ((te,obs):xs)) = [Estrada 0, Rio 0]
                                 | otherwise = [Rio 0, Estrada 0, Relva]

{-funcao aprova de bala-}
{-proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva] 
proximosTerrenosValidos mapa@(Mapa l ((te,obs):xs))| tipodeobs mapa && terrenoscontiguos mapa && isrioFIM mapa = [Estrada 0, Relva]
                                                   | tipodeobs mapa && terrenoscontiguos mapa && isestradaFIM mapa = [Rio 0, Relva]
                                                   | tipodeobs mapa && terrenoscontiguos mapa && isrelvaFIM mapa = [Estrada 0, Rio 0]
                                                   | otherwise = [Rio 0, Estrada 0, Relva]
-}
{-| Funcao que verifica se temos o numero limite de Rios-}
isrioFIM :: Mapa -> Bool
isrioFIM mapa = isRio2 1 mapa

isRio2 :: Int -> Mapa -> Bool
isRio2 _ (Mapa _ []) = False
isRio2 4 (Mapa _ ((te,obs):xs)) = True 
isRio2 n (Mapa l ((te,obs):xs)) | inicio te == "Rio" = isRio2 (n+1)  (Mapa l (xs))
                                | otherwise = False

{-| Funcao que verifica se temos o numero limite de estradas-}
isestradaFIM :: Mapa -> Bool
isestradaFIM mapa = isEstrada2 1 mapa

isEstrada2 :: Int -> Mapa -> Bool
isEstrada2 _ (Mapa _ []) = False
isEstrada2 5 (Mapa _ ((te,obs): xs)) = True
isEstrada2 n (Mapa l ((te,obs): xs)) | inicio te == "Est" = isEstrada2 (n+1) (Mapa l (xs))
                                     | otherwise = False

{-| Funcao que verifica se temos o numero limite de Relva-}                                    
isrelvaFIM :: Mapa -> Bool
isrelvaFIM mapa = isRelva2 1 mapa

isRelva2 :: Int -> Mapa -> Bool
isRelva2 _ (Mapa _ []) = False 
isRelva2 5 (Mapa _ ((te,obs):xs)) = True
isRelva2 n (Mapa l ((te,obs):xs)) | inicio te == "Rel" = isRelva2 (n+1) (Mapa l (xs))
                                  | otherwise = False


{-|Funcao que verifica os possiveis proximos obstaculos validos-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos _ (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos _ (Estrada vel, []) = [Nenhum,Carro]
proximosObstaculosValidos n (te, (x:xs)) | inicio te == "Rio" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]
                                         | inicio te == "Rio" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum,Tronco]                     
                                         | inicio te == "Rel" && tipobs (te, (x:xs)) && n > length (x:xs) && not ( elem Nenhum (x:xs)) = [Nenhum]
                                         | inicio te == "Rel" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum, Arvore]
                                         | inicio te == "Est" && tipobs (te, (x:xs)) && 
                                         n > length (x:xs) && not ( elem Nenhum (x:xs)) = [Nenhum]
                                         | inicio te == "Est" && tipobs (te, (x:xs)) && 
                                         n > length (x:xs) = [Nenhum, Carro]
                                         | otherwise = []

{-| Funcao que verifica se os obstaculos correspondem ao tipo de Terreno-}
tipobs :: (Terreno,[Obstaculo]) -> Bool
tipobs (_, []) = True
tipobs (te, (x:xs))
  | inicio te == "Rel" && x == Arvore || x == Nenhum = tipobs (te, xs) 
  | inicio te == "Rio" && x == Tronco || x == Nenhum = tipobs (te, xs)
  | inicio te == "Est" && x == Carro  || x == Nenhum = tipobs (te, xs)
  | otherwise = False



{-Funcao que verifica se o terreno e Rio-}
{-(para os casos em que temos que verificar se o nenum faz parte) proximosObstaculosValidos n (te, (x:xs)) | tiposdeobs (Mapa n (te, (x:xs))) && n > length (x:xs) && isRio (te, (x:xs)) && elem Nenhum (x:xs) = [Nenhum,Tronco] -}




{-
randomIntsL :: Int -> Int -> [Int]
randomIntsL seed len = take len (randoms (mKStdGen seed))
-}

