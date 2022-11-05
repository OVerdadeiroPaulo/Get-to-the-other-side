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
