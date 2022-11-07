{- |
Module      : Tarefa1_2022li1g088
Description : Validação de um mapa
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g088 where

import LI12223

mapaValido :: Mapa -> Bool
mapaValido mapa@(Mapa _ (((_, listadeobs):xs))) 
  | vervazios listadeobs && vernrobstaculos mapa && tipodeobs mapa && riospostos mapa && troncoline 0 mapa && carroline 0 mapa && terrenosseguidos 0 mapa = True
  | otherwise = False


{-|Funcao que verifica que tem algum espaço com Nenhum obstaculo-}
vervazios :: [Obstaculo] -> Bool
vervazios [] = False
vervazios (x:xs) 
  | x== Nenhum = True
  |otherwise = vervazios xs
{-|funcao que valida que a largura é do tamanho da lista de obstaculos-}
vernrobstaculosfailed :: Mapa -> Bool
vernrobstaculosfailed (Mapa l ((_ , k):xs)) = l == length k


vernrobstaculos :: Mapa -> Bool
vernrobstaculos (Mapa l []) = True
vernrobstaculos (Mapa l ((_ , k):xs)) 
  | l == length k = vernrobstaculos (Mapa l (xs))
  | otherwise = False
{-|funcao que verifica se o Terreno tem algum Obstaculo nao permitido-}
tipodeobs :: Mapa -> Bool
tipodeobs (Mapa larg ([(terr, [])])) = True
tipodeobs (Mapa larg ([])) = True
tipodeobs (Mapa larg (((Relva, (x:xs)):ys)))
  | x == Arvore || x== Nenhum = tipodeobs (Mapa larg ((ys)))
  | otherwise = False
tipodeobs (Mapa larg ([(Rio vel, (x:xs))]))
  | x == Carro || x== Arvore = False
  | otherwise = tipodeobs (Mapa larg ([(Rio vel, (xs))]))
tipodeobs (Mapa larg ([(Estrada vel, (x:xs))]))
  | x == Arvore || x== Tronco = False
  | otherwise = tipodeobs (Mapa larg ([(Estrada vel, (xs))]))

{-|funcao que valida que rios contiguos tem velocidade oposta-}

riospostos :: Mapa -> Bool
riospostos (Mapa larg ([])) = True
{-por exepcoes para outros tipos de terreno-}

riospostos (Mapa larg (((Rio vel1, obst):(Rio vel2, obs):xs)))
  | vel1 * vel2 >= 0 = False
  | otherwise = riospostos (Mapa larg ((xs)))
riospostos _ = True
{-|funcao que valida o comprimento dos obstaculos(troncos) -}
troncoline :: Int -> Mapa -> Bool
troncoline _ (Mapa larg ([])) = True
troncoline _ (Mapa larg ([(terr, [])])) = True
troncoline 5 (Mapa larg ([(terr, (x:xs))])) = False
troncoline k (Mapa larg ([(terr, (x:xs))])) 
  | x== Tronco = troncoline (k + 1) (Mapa larg ([(terr, (xs))])) 
  | otherwise = troncoline (0) (Mapa larg ([(terr, (xs))])) 

{-|funcao que valida o comprimento dos obstaculos(carros) -}
carroline :: Int -> Mapa -> Bool
carroline _ (Mapa larg ([(terr, [])])) = True
carroline 3 (Mapa larg ([(terr, (x:xs))])) = False
carroline k (Mapa larg ([(terr, (x:xs))])) 
  | x== Carro = carroline (k + 1) (Mapa larg ([(terr, (xs))])) 
  | otherwise = carroline (0) (Mapa larg ([(terr, (xs))])) 


{-|tentativ FALHADA de funcao que valida se nao ha demasiados do mesmo tipo de terreno seguidos-}
terrenoseguidosfail :: Mapa -> Bool 
terrenoseguidosfail (Mapa larg (((Rio vel, obs1):(Rio vel2, obs2):(Rio vel3, obs3):(Rio vel4, obs4):xs))) = False
terrenoseguidosfail (Mapa larg (((Relva, obs1):(Relva, obs2):(Relva, obs3):(Relva, obs4):(Relva, obs5) :xs))) = False
terrenoseguidosfail (Mapa larg (((Estrada vel, obs1):(Estrada vel2, obs2):(Estrada vel3, obs3):(Estrada vel4, obs4):(Estrada vel5, obs5) :xs))) = False
terrenoseguidosfail (Mapa larg (((terr, obs):xs))) = True
{-|funcao sucedida que valida se na ha demasiados terrenos do mesmo tipo-}
terrenosseguidos :: Int -> Mapa -> Bool
terrenosseguidos _ (Mapa _ ([])) = True
terrenosseguidos 4 (Mapa l (((Rio _, _):xs))) = False
terrenosseguidos k (Mapa l (((Rio _, _):xa:xs))) =
     case xa of 
      (Rio _, _) -> terrenosseguidos (k+1) (Mapa l ((xa:xs)))
      _ -> terrenosseguidos 0 (Mapa l ((xa:xs)))
terrenosseguidos 5 (Mapa l (((Estrada _, _):xs))) = False
terrenosseguidos k (Mapa l (((Estrada _, _):xa:xs))) =
     case xa of 
      (Estrada _, _) -> terrenosseguidos (k+1) (Mapa l ((xa:xs)))
      _ -> terrenosseguidos 0 (Mapa l ((xa:xs)))
terrenosseguidos 5 (Mapa l (((Relva, _):xs))) = False
terrenosseguidos k (Mapa l (((Relva, _):xa:xs))) =
     case xa of 
      (Relva, _) -> terrenosseguidos (k+1) (Mapa l ((xa:xs)))
      _ -> terrenosseguidos 0 (Mapa l ((xa:xs)))

