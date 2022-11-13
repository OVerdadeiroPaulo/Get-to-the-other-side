module Tarefa2_2022li1g088_Spec where

import LI12223
import Tarefa2_2022li1g088
import Tarefa1_2022li1g088
import Tarefa3_2022li1g088
import Tarefa4_2022li1g088
import Test.HUnit


testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste1estendeMapa1 " ~: Mapa 4 [(Rio 3,[Nenhum,Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum])] ~=? (estendeMapa estendeMapatest1 18),"Teste2estendeMapa2 " ~: Mapa 2 [(Rio (-2),[Tronco,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])] ~=? (estendeMapa estendeMapatest2 3),"Test3estendeMapa3 " ~: Mapa 2 [(Relva,[Nenhum,Arvore]),(Estrada (-2),[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])] ~=? (estendeMapa estendeMapatest3 15),"Teste4estendeMapa4 " ~: Mapa 3 [(Estrada (-4),[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Rio 3,[Tronco,Tronco,Nenhum])] ~=? (estendeMapa estendeMapatest4 9),"Teste5estendeMapa5 " ~: Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])] ~=? (estendeMapa estendeMapatest5 67),"Teste6proximosTerrenosValidos1 " ~: [Rio 0,Estrada 0,Relva] ~=? (proximosTerrenosValidos proximosTerrenosValidostest1),"Teste7proximosTerrenosValidos2 " ~: [Estrada 0,Relva] ~=? (proximosTerrenosValidos proximosTerrenosValidostest2),"Teste8proximosTerrenosValidos3 " ~: [Rio 0,Relva] ~=? (proximosTerrenosValidos proximosTerrenosValidostest3),"Teste9proximosTerrenosValidos4 " ~: [Estrada 0,Rio 0] ~=? (proximosTerrenosValidos proximosTerrenosValidostest4),"Teste10proximosTerrenosValidos5 " ~: [Rio 0,Estrada 0,Relva] ~=? (proximosTerrenosValidos proximosTerrenosValidostest5),"Teste11proximosObstaculos6 " ~: [] ~=? (proximosTerrenosValidos proximosTerrenosValidostest6),"Teste12proximosObstaculos1 " ~: [Nenhum,Tronco] ~=?(proximosObstaculosValidos 5 proximosObstaculosValidos1),"Testes13proximosObstaculos2 " ~: [Nenhum,Carro] ~=?(proximosObstaculosValidos 3 proximosObstaculosValidos2),"Testes14proximosObstaculos3 " ~: [Nenhum] ~=? (proximosObstaculosValidos 4 proximosObstaculosValidos3),"Testes15proximosObstaculos4" ~: [Tronco] ~=? (proximosObstaculosValidos 3 proximosObstaculosValidos4)]