{- |
Module      : Tarefa5_2022li1g088
Description : Funcao para fazer o mapa deslizar continuamente 
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}

module Tarefa5_2022li1g088 where

import LI12223
import Tarefa1_2022li1g088
import Tarefa2_2022li1g088
import Tarefa3_2022li1g088
import Tarefa4_2022li1g088
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

{-| Funcao 'deslizaJogo'. Esta funcao com a estendeMapa como auxiliar, retira a ultima linha do mapa, e gera um mapa com uma nova linha na frente preservando assim o mesmo tamanho 

== codigo :
@
deslizaJogo :: Int -> Jogo -> Jogo 
deslizaJogo a (Jogo j m@(Mapa l ((te,obs):xs))) | (mod a 300) < 1  = (Jogo (deslocajogador j (Move Baixo) m) (estendeMapa (Mapa l (init ((te,obs):xs))) a))
                                                | otherwise = (Jogo j (Mapa l ((te,obs):xs)))
@-}

deslizaJogo :: Int -> Jogo -> Jogo 
deslizaJogo a (Jogo j m@(Mapa l ((te,obs):xs))) | (mod a 300) < 150  = (Jogo (deslocajogador j (Move Baixo) m) (estendeMapa (Mapa l (init ((te,obs):xs))) a))
                                                | otherwise = (Jogo j (Mapa l ((te,obs):xs)))

deslizaJogo2 :: Int -> Jogo -> Jogo 
deslizaJogo2 a (Jogo j m@(Mapa l ((te,obs):xs))) = (Jogo (deslocajogador j (Move Baixo) m) (estendeMapa (Mapa l (init ((te,obs):xs))) a))
                                                