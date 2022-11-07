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

{-usar funcao da tarefa um 7-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa la te) 
        | terrenosSeguidos (Mapa _ te) && te == [(Rio _, _),(Rio _, _),(Rio _, _),(Rio _, _)] = [Estrada 0, Relva] 
        | terrenosSeguidos (Mapa _ te) && te == [(Estrada _, _),(Estrada _, _),(Estrada _, _),(Estrada _, _),(Estrada _, _)] = [ Relva, Rio 0]
        | terrenosSeguidos (Mapa _ te) && te == [(Relva, _),(Relva, _),(Relva, _),(Relva, _)] = [Estrada 0, Rio 0]
        | otherwise = [Rio 0, Estrada 0, Relva]



proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos 
proximosObstaculosValidos _ (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos _ (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos _ (Estrada, []) = [Nenhum,Carro]
proximosObstaculosValidos n (te, (x:xs)) | n > length (x:xs) &&  (Rio _, (x:xs)) = [Nenhum,Tronco]
                                         | n > length (x:xs) &&  (Relva , (x:xs)) = [Nenhum, Arvore]
                                         | n > length (x:xs) &&  (Estrada _, (x:xs)) = [Nenhum, Carro]
                                         | otherwise = []


proximosObstaculosValidos n (te, (x:xs)) | topipodepbs (Mapa _ (te,(c:xs):ys) && n > length (x:xs) &&  (Rio _, (x:xs)) = [Nenhum,Tronco]



        
