module Tarefa3_2022li1g088_Spec where

import LI12223
import Tarefa3_2022li1g088
import Tarefa1_2022li1g088
import Tarefa2_2022li1g088
import Tarefa4_2022li1g088
import Test.HUnit
import LI12223 (Direcao(Direita, Esquerda))

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste Normal" ~: (Jogo (Jogador (0,0)) (Mapa 2 [(Relva,[Nenhum,Nenhum,Carro]),(Estrada 2,[Nenhum,Carro,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum])])) ~=? (animaJogo jogoNormal (Move Cima)),"Teste Arvores" ~: (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum])])) ~=? (animaJogo jogoImpossivelMoverArvore (Move Cima)),"Teste Limites do mapa" ~: (Jogo (Jogador (0,0)) (Mapa 1 [(Rio 2,[Nenhum])])) ~=? (animaJogo jogoImpossivelLimitesMapa (Move Cima)),"Teste Tronco" ~: (Jogo (Jogador (0,0)) (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Nenhum])])) ~=? (animaJogo jogoTronco   (Parado)),"Teste pedido1" ~: (Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 2, [Nenhum, Carro, Nenhum])])) ~=? (animaJogo (Jogo(Jogador(1,0))(Mapa 3 [(Estrada 2, [Carro, Nenhum, Nenhum])]))   (Parado)),"Teste pedido2" ~: (Jogo (Jogador (0,0)) (Mapa 3 [(Estrada 2, [Carro, Nenhum, Nenhum])])) ~=? (animaJogo (Jogo(Jogador(1,0))(Mapa 3 [(Estrada 2, [Carro, Nenhum, Nenhum])]))   (Move Esquerda)),"Teste pedido4" ~: (Jogo (Jogador (1, 0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]) ,(Rio 1, [Tronco, Nenhum, Tronco]) ,(Relva, [Nenhum, Nenhum, Nenhum])])) ~=? (animaJogo (Jogo(Jogador(1,1))(Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]) ,(Rio 1, [Nenhum, Tronco, Tronco]) ,(Relva, [Nenhum, Nenhum, Nenhum])]))   (Move Cima)),"Teste pedido5" ~: (Jogo (Jogador (3, 1)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]) ,(Rio 1, [Tronco, Nenhum, Tronco]) ,(Relva, [Nenhum, Nenhum, Nenhum])])) ~=? (animaJogo (Jogo(Jogador(1,1))(Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]) ,(Rio 1, [Nenhum, Tronco, Tronco]) ,(Relva, [Nenhum, Nenhum, Nenhum])]))   (Move Direita))]
