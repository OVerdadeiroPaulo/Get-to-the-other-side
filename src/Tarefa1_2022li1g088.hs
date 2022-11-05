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


{-mapaValido Mapa larg ((Relva), (x:xy))
  | x == Carro || x== Tronco = False
  | othewise = mapaValido xs
   larg == length (x:xy) -}
vervazios :: [Obstaculo] -> Bool
vervazios [] = False
vervazios (x:xs) 
  | x== Nenhum = True
  |otherwise = vervazios xs

vernrobstaculos :: Mapa -> Bool
vernrobstaculos (Mapa l [(_ , k)]) = l == length k
