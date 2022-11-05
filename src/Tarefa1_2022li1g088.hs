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
mapaValido = undefined


{-|Funcao que verifica que tem algum espaço com Nenhum obstaculo-}
vervazios :: [Obstaculo] -> Bool
vervazios [] = False
vervazios (x:xs) 
  | x== Nenhum = True
  |otherwise = vervazios xs
{-|funcao que valida que a largura é do tamanho da lista de obstaculos-}
vernrobstaculos :: Mapa -> Bool
vernrobstaculos (Mapa l [(_ , k)]) = l == length k
{-|funcao que verifica se o Terreno tem algum Obstaculo nao permitido-}
tipodeobs :: Mapa -> Bool
tipodeobs (Mapa larg ([(terr, [])])) = True
tipodeobs (Mapa larg ([(Relva, (x:xs))]))
  | x == Carro || x== Tronco = False
  | otherwise = tipodeobs (Mapa larg ([(Relva, (xs))]))
tipodeobs (Mapa larg ([(Rio vel, (x:xs))]))
  | x == Carro || x== Arvore = False
  | otherwise = tipodeobs (Mapa larg ([(Rio vel, (xs))]))
tipodeobs (Mapa larg ([(Estrada vel, (x:xs))]))
  | x == Arvore || x== Tronco = False
  | otherwise = tipodeobs (Mapa larg ([(Estrada vel, (xs))]))

{-|funcao que valida que rios contiguos tem velocidade oposta-}

riospostos :: Mapa -> Bool
riospostos (Mapa larg ([])) = True
riospostos (Mapa larg (((Rio vel1, obst):(Rio vel2, obs):xs)))
  | vel1 * vel2 >= 0 = False
  | otherwise = riospostos (Mapa larg ((xs)))

{-|funcao que valida o comprimento dos obstaculos(troncos) -}
troncoline :: Int -> Mapa -> Bool
troncoline 5 (Mapa larg ([(terr, (x:xs))])) = False
troncoline k (Mapa larg ([(terr, (x:xs))])) 
  | x== Tronco = troncoline (k + 1) (Mapa larg ([(terr, (xs))])) 
  | otherwise = troncoline (0) (Mapa larg ([(terr, (xs))])) 
     where k = 0
{-|funcao que valida o comprimento dos obstaculos(carros) -}
carroline :: Int -> Mapa -> Bool
carroline 3 (Mapa larg ([(terr, (x:xs))])) = False
carroline k (Mapa larg ([(terr, (x:xs))])) 
  | x== Carro = carroline (k + 1) (Mapa larg ([(terr, (xs))])) 
  | otherwise = carroline (0) (Mapa larg ([(terr, (xs))])) 
     where k = 0

{-|tentativ FALHADA de funcao que valida se nao ha demasiados do mesmo tipo de terreno seguidos-}
terrenoseguidos :: Mapa -> Bool 
terrenoseguidos (Mapa larg (((Rio vel, obs1):(Rio vel2, obs2):(Rio vel3, obs3):(Rio vel4, obs4):xs))) = False
terrenoseguidos (Mapa larg (((Relva, obs1):(Relva, obs2):(Relva, obs3):(Relva, obs4):(Relva, obs5) :xs))) = False
terrenoseguidos (Mapa larg (((Estrada vel, obs1):(Estrada vel2, obs2):(Estrada vel3, obs3):(Estrada vel4, obs4):(Estrada vel5, obs5) :xs))) = False
terrenoseguidos (Mapa larg (((terr, obs):xs))) = True

