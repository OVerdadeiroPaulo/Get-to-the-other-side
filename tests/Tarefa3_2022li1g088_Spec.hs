module Tarefa3_2022li1g088_Spec where

import LI12223
import Tarefa3_2022li1g088
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste Normal" ~: (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 2,[Nenhum,Carro,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum])])) ~=? (animaJogo jogoNormal (Move Cima)),"Teste Arvores" ~: (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum])])) ~=? (animaJogo jogoImpossivelMoverArvore (Move Cima)),"Teste Limites do mapa" ~: (Jogo (Jogador (0,0)) (Mapa 1 [(Estrada 2,[Nenhum])])) ~=? (animaJogo jogoImpossivelLimitesMapa (Move Cima)),"Teste Tronco" ~: (Jogo (Jogador (0,0)) (Mapa 3 [(Rio (-1),[Tronco,Nenhum,Nenhum])])) ~=? (animaJogo jogoTronco   (Parado))]
