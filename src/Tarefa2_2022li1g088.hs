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


{-
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa la linhas@(x:xs)) | terreno == Rio 0 = (Mapa la ((Rio ve, obstaculos):xs)) 
                                    | terreno == Estrada 0 = (Mapa la ((Estrada ve, obstaculos):xs))
                                    | terreno == Relva = (Mapa la ((relva, obstaculos):xs))
            where obstaculos = proximosObstaculosValidos la  x 
--                  terreno = proximosTerrenosValidos (Mapa la linhas @(x:xs))
                  ve = (head randomIntsL)

unlist::[a] -> a
unlist (x:xs) =  x
randomIntsL :: Int -> Int -> [Int]
<<<<<<< HEAD
randomIntsL seed len = take len (randoms (mkStdGen seed))
aleatorio100 :: Int -> Int
aleatorio100 k = abs ((unlist(randomIntsL (k) (1) )) `mod` (100))

=======
randomIntsL seed len = take len (randoms (mKStdGen seed))
-}
>>>>>>> 19e6cbb4b433ba5ffbed899c97e17dfa7509f1a6

{-Funcao que verifica os proximos terrenos validos-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa l ((te,obs):xs))| isrioFIM (Mapa l ((te,obs):xs)) ==True = [Estrada 0, Relva]
                                              | isestradaFIM (Mapa l ((te,obs):xs)) == True = [Rio 0, Relva]
                                              | isrelvaFIM (Mapa l ((te,obs):xs)) == True = [Estrada 0, Rio 0]
                                              | otherwise = [Rio 0, Estrada 0, Relva]


isrioFIM :: Mapa -> Bool
isrioFIM mapa = isRio2 1 mapa

isRio2 :: Int -> Mapa -> Bool
isRio2 _ (Mapa _ []) = False
isRio2 4 (Mapa _ ((te,obs):xs)) = True 
isRio2 n (Mapa l ((te,obs):xs)) | inicio te == "Rio" = isRio2 (n+1)  (Mapa l (xs))
                                | otherwise = False

isestradaFIM :: Mapa -> Bool
isestradaFIM mapa = isEstrada2 1 mapa

isEstrada2 :: Int -> Mapa -> Bool
isEstrada2 _ (Mapa _ []) = False
isEstrada2 5 (Mapa _ ((te,obs): xs)) = True
isEstrada2 n (Mapa l ((te,obs): xs)) | inicio te == "Est" = isEstrada2 (n+1) (Mapa l (xs))
                                     | otherwise = False
isrelvaFIM :: Mapa -> Bool
isrelvaFIM mapa = isRelva2 1 mapa

isRelva2 :: Int -> Mapa -> Bool
isRelva2 _ (Mapa _ []) = False 
isRelva2 5 (Mapa _ ((te,obs):xs)) = True
isRelva2 n (Mapa l ((te,obs):xs)) | inicio te == "Rel" = isRelva2 (n+1) (Mapa l (xs))
                                  | otherwise = False

{-
{-|Funcao que verifica os possiveis proximos obstaculos validos-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos _ (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos _ (Estrada vel, []) = [Nenhum,Carro]
proximosObstaculosValidos n (te, (x:xs)) | inicio te == "Rio" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum,Tronco] 
                                         | tipobs (Mapa n (((te, (x:xs)):ys))) && n > length (x:xs) &&  isRelva' = [Nenhum, Arvore]
                                         | tipobs (Mapa n (((te, (x:xs)):ys))) && 
                                         n > length (x:xs) &&  isEstrada' = [Nenhum, Carro]
                                         | otherwise = []
-}

tipobs :: (Terreno,[Obstaculo]) -> Bool
tipobs (_, []) = True
tipobs (te, (x:xs))
  | inicio te == "Rel" && x == Arvore || x == Nenhum = tipobs (te, xs) 
  | inicio te == "Rio" && x == Tronco || x == Nenhum = tipobs (te, xs)
  | inicio te == "Est" && x == Carro  || x == Nenhum = tipobs (te, xs)
  | otherwise = False



{-(para os casos em que nao temos nenhuma opcao com o nenhum)  |tiposdeobs (Mapa n (te, (x:xs))) && (n-1) == length (x:xs) && isRio (te, (x:xs)) = [Nenhum] -}
{-Funcao que verifica se o terreno e Rio-}
{-(para os casos em que temos que verificar se o nenum faz parte) proximosObstaculosValidos n (te, (x:xs)) | tiposdeobs (Mapa n (te, (x:xs))) && n > length (x:xs) && isRio (te, (x:xs)) && elem Nenhum (x:xs) = [Nenhum,Tronco] -}


{-
isRio' :: (Terreno,[Obstaculo]) -> Bool
isRio' (te, _) = case te of 
                        (Rio _) -> True
                        _ -> False

{-Funcao que verifica se o terreno e Estrada-}
isEstrada' :: (Terreno,[Obstaculo]) -> Bool
isEstrada' (te, _) = case te of 
                             (Estrada _) -> True
                             _ -> False

{-Funcao que verifica se o terreno e Relva-}
isRelva' :: (Terreno,[Obstaculo]) -> Bool
isRelva' (te, _) = case te of 
                           (Relva) -> True
                           _ -> False
-}


<<<<<<< HEAD
=======
{-
randomIntsL :: Int -> Int -> [Int]
randomIntsL seed len = take len (randoms (mKStdGen seed))
-}

>>>>>>> 19e6cbb4b433ba5ffbed899c97e17dfa7509f1a6
