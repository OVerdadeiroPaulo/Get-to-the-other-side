{- |
Module      : Tarefa2_2022li1g088
Description : Geração contínua de um mapa
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g088 where

import LI12223

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa = undefined


proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((te,obs):xs) | isRio (Mapa _ ((te,obs):xs)) = [Estrada 0, Relva]
                                              | isEstrada (Mapa _ ((te,obs):xs)) = [Rio 0, Relva]
                                              | isRelva (Mapa _ ((te,obs):xs)) = [Estrada 0, Rio 0]
                                              | otherwise = [Rio 0, Estrada 0, Relva]


isRio :: Int -> Mapa -> Bool
isRio 0 (Mapa _ _) = True
isRio _ (Mapa _ []) = False
isRio 4 (Mapa _ ((te,obs):xs)) = case te of 
                                         (Rio _) -> isRio (n-1) xs
                                         _ -> False

isEstrada :: Int -> Mapa -> Bool
isEstrada 0 (Mapa _ _) = True
isEstrada _ (Mapa _ []) = False
isEstrada 5 (Mapa _ ((te,obs):xs)) = case te of 
                                             (Estrada _) -> isEstrada (n-1) xs
                                             _ -> False

isRelva :: Int -> Mapa -> Bool
isRelva 0 (Mapa _ _) = True
isRelva _ (Mapa _ []) = False
isRelva 5 (Mapa _ ((te,obs):xs)) = case te of 
                                           (Relva _) -> isRelva (n-1) xs
                                           _ -> False


{-usar funcao da tarefa um 7
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa la te) 
        | terrenosSeguidos (Mapa _ te) && te == ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):xs) = [Estrada 0, Relva] 
        | terrenosSeguidos (Mapa _ te) && te == ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):xs) = [ Relva, Rio 0]
        | terrenosSeguidos (Mapa _ te) && te == ((Relva, _):(Relva, _):(Relva, _):(Relva, _):xs) = [Estrada 0, Rio 0]
        | otherwise = [Rio 0, Estrada 0, Relva] -}

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos _ (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos _ (Estrada, []) = [Nenhum,Carro]
proximosObstaculosValidos n (te, (x:xs)) | tiposdeobs (Mapa n (te, (x:xs))) && n > length (x:xs) && (Rio _, (x:xs)) = [Nenhum,Tronco]
                                         | tiposdeobs (Mapa n (te, (x:xs))) && n > length (x:xs) &&  (Relva , (x:xs)) = [Nenhum, Arvore]
                                         | tiposdeobs (Mapa n (te, (x:xs))) && 
                                         n > length (x:xs) &&  (Estrada _, (x:xs)) = [Nenhum, Carro]
                                         | otherwise = []





        
