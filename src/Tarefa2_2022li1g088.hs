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
randomIntsL seed len = take len (randoms (mkStdGen seed))
aleatorio100 :: Int -> Int
aleatorio100 k = abs ((unlist(randomIntsL (k) (1) )) `mod` (100))


{-Funcao que verifica os proximos terrenos validos-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((te,obs):xs))| isRio (Mapa _ ((te,obs):xs)) = [Estrada 0, Relva]
                                              | isEstrada (Mapa _ ((te,obs):xs)) = [Rio 0, Relva]
                                              | isRelva (Mapa _ ((te,obs):xs)) = [Estrada 0, Rio 0]
                                              | otherwise = [Rio 0, Estrada 0, Relva]


{-auxiliar para silverio-}
terrenoscontiguos2 :: Mapa -> Bool
terrenoscontiguos2 (Mapa _ (([]))) = False
terrenoscontiguos2 mapa@(Mapa l (((x):xs)))
  | inicio (fst(head (head (agrupaterrenos mapa)))) == "Est" && length (head (agrupaterrenos mapa)) >5 || inicio ( (head (agrupaterrenos mapa))) == "Rel" && length (head (agrupaterrenos mapa))  > 5 = True
  | inicio (fst(head a)) == "Rio" && length (head (agrupaterrenos mapa)) > 4 = True
  | otherwise = terrenoscontiguos2 (Mapa l (((xs))))
      where (a:b) = agrupaterrenos mapa


isrioFIM :: Mapa -> Bool
isrioFIM mapa = isRio2 0 mapa
isRio2 :: Int -> Mapa -> Bool
isRio2 _ (Mapa _ []) = False
isRio2 4 (Mapa _ ((te,obs):xs)) = True 
isRio2 k (Mapa l ((te,obs):xs)) 
  | inicio te == "Rio" = isRio2 (k+1)  (Mapa l ((xs)))


{-Funcao que verifica se temos 4 rios para a main proximosTerrenosValidos-}
isRio :: Int -> Mapa -> Bool
isRio 0 (Mapa _ _) = True
isRio _ (Mapa _ []) = False
isRio 4 (Mapa _ ((te,obs):xs)) =  case te of 
                                         (Rio _) -> isRio (n-1) xs
                                         _ -> False

{-Funcao que verifica se temos 5 Estradas para a main proximosTerrenosValidos-}
isEstrada :: Int -> Mapa -> Bool
isEstrada 0 (Mapa _ _) = True
isEstrada _ (Mapa _ []) = False
isEstrada 5 (Mapa _ ((te,obs):xs)) = case te of 
                                             (Estrada _) -> isEstrada (n-1) xs
                                             _ -> False

{-Funcao que verifica se temos 5 Relvax para a main proximosTerrenosValidos-}
isRelva :: Int -> Mapa -> Bool
isRelva 0 (Mapa _ _) = True
isRelva _ (Mapa _ []) = False
isRelva 5 (Mapa _ ((te,obs):xs)) = case te of 
                                           (Relva ) -> isRelva (n-1) xs
                                           _ -> False

{-Funcao que verifica os possiveis proximos obstaculos validos-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos _ (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos _ (Estrada vel, []) = [Nenhum,Carro]
proximosObstaculosValidos n (te, (x:xs)) | obsemlinha (Mapa n (te, (x:xs))) && n > length (x:xs) && isRio (te, (x:xs)) = [Nenhum,Tronco] 
                                         | obsemlinha (Mapa n (te, (x:xs))) && n > length (x:xs) &&  isRelva' = [Nenhum, Arvore]
                                         | obsemlinha (Mapa n (te, (x:xs))) && 
                                         n > length (x:xs) &&  isEstrada' = [Nenhum, Carro]
                                         | otherwise = []
{-(para os casos em que nao temos nenhuma opcao com o nenhum)  |tiposdeobs (Mapa n (te, (x:xs))) && (n-1) == length (x:xs) && isRio (te, (x:xs)) = [Nenhum] -}
{-Funcao que verifica se o terreno e Rio-}
{-(para os casos em que temos que verificar se o nenum faz parte) proximosObstaculosValidos n (te, (x:xs)) | tiposdeobs (Mapa n (te, (x:xs))) && n > length (x:xs) && isRio (te, (x:xs)) && elem Nenhum (x:xs) = [Nenhum,Tronco] -}


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

