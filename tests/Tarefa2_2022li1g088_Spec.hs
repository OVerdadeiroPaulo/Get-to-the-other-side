module Tarefa2_2022li1g088_Spec where

import LI12223
import Tarefa2_2022li1g088
import Tarefa1_2022li1g088
import Tarefa3_2022li1g088
import Tarefa4_2022li1g088
import Test.HUnit

data Test = TestCase Assertion
            | TestList [Test]
            | TestLabel String Test
--testsT2 :: Test
--testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1" ~: 1 ~=? 1]
{-|Testes para a Funcao estendeMapa-}
teste1 = TestCase (assertEqual "for estendeMapa Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum,Nenhum]),(Rio (-2),[Nenhum,Tronco,Nenhum]),(Rio 2,[Tronco,Nenhum,Tronco]),(Rio (-3),[Nenhum,Tronco,Tronco])]  (estendeMapa (Mapa 3 [(Rio 1, [Tronco,Nenhum,Nenhum]) " Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum,Nenhum]),(Rio (-2),[Nenhum,Tronco,Nenhum]),(Rio 2,[Tronco,Nenhum,Tronco]),(Rio (-3),[Nenhum,Tronco,Tronco])]  (estendeMapa (Mapa 3 [(Rio 1, [Tronco,Nenhum,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum]),(Rio 2, [Tronco,Nenhum,Tronco]),(Rio (-3), [Nenhum,Tronco,Tronco])]) 5
teste2 = TestCase (assertEqual "for estendeMapa Mapa 3 [(Estrada (-4),[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Rio 3,[Tronco,Tronco,Nenhum])]" Mapa 3 [(Estrada (-4),[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Rio 3,[Tronco,Tronco,Nenhum])] (estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Rio 3, [Tronco,Tronco,Nenhum])]) 9))
teste3 = TestCase (assertEqual "for estendeMapa Mapa 3 [(Estrada 2,[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]" Mapa 3 [(Estrada 2,[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])] (estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Relva, [Arvore,Nenhum,Arvore])]) 1))
teste4 = TestCase (assertEqual "for estendeMapa Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]" Mapa 3 [(Relva,[Arvore,Arvore,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])] (estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Relva, [Arvore,Nenhum,Arvore])]) 67))
teste5 = TesteCase (assertEqual "for estendeMapa Mapa 3 [(Estrada 2,[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]" Mapa 3 [(Estrada 2,[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])] (estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Relva, [Arvore,Nenhum,Arvore])]) 1))
teste6 = TestCase (assertEqual "for estendeMapa Mapa 3 [(Estrada 4,[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]" Mapa 3 [(Estrada 4,[Carro,Carro,Nenhum]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])] (estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Relva, [Arvore,Nenhum,Arvore])]) 2))
testes7 = Testcase (assertEqual "for estendeMapa Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]" Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Rio (-2),[Tronco,Nenhum,Tronco]),(Rio 3,[Tronco,Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])] (estendeMapa (Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Relva, [Arvore,Nenhum,Arvore])]) 101)) 
testes8 = TestCase (assertEqual "for estendeMapa Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Nenhum,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum])]" Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Nenhum,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum])] (estendeMapa (Mapa 4 [(Rio (-1),[Nenhum,Tronco,Nenhum,Tronco]),(Rio 4, [Tronco,Nenhum,Tronco,Nenhum]),(Relva, [Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum])]) 17)) 
testes9 = TestCase (assertEqual "for estendeMapa Mapa 4 [(Rio 2,[Tronco,Tronco,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum,Carro]),(Estrada 4,[Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum])]" Mapa 4 [(Rio 2,[Tronco,Tronco,Tronco,Nenhum]),(Estrada (-1),[Nenhum,Carro,Nenhum,Carro]),(Estrada 4,[Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum])] (estendeMapa (Mapa 4 [(Estrada (-1),[Nenhum,Carro,Nenhum,Carro]),(Estrada 4,[Carro,Nenhum,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum])]) 1))
testes10 = TestCase (assertEqual "for estendeMapa " Mapa 4 [(Rio 3,[Nenhum,Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro])] (estendeMapa (Mapa 4 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro])]) 18)) 
testes11 = TestCase (assertEqual "for estendeMapa Mapa 5 [(Estrada (-4),[Carro,Carro,Carro,Carro,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Carro])]" Mapa 5 [(Estrada (-4),[Carro,Carro,Carro,Carro,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Carro])] (estendeMapa (Mapa 5 [(Rio (-1),[Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Carro])]) 19))
testes12 = TestCase (assertEqual "for estendeMapa Mapa 5 [(Rio 3,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio (-2),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])]" Mapa 5 [(Rio 3,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco]),(Rio (-2),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])] (estendeMapa (Mapa 5 [(Rio (-2),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])]) 40))
testes13 = TestCase (assertEqual "for estendeMapa Mapa 2 [(Relva,[Nenhum,Arvore]),(Estrada (-2),[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])]" Mapa 2 [(Relva,[Nenhum,Arvore]),(Estrada (-2),[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])] (estendeMapa (Mapa 2 [(Estrada (-2),[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])]) 15
))
testes14 = TestCase (assertEqual "for estendeMapa Mapa 2 [(Rio (-2),[Tronco,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])]" Mapa 2 [(Rio (-2),[Tronco,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])] (estendeMapa (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])]) 3
))
testes15 = TestCase (assertEqual "for estendeMapa Mapa 2 [(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])]" Mapa 2 [(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])] (estendeMapa (Mapa 2 [(Rio 1,[Tronco,Nenhum]),(Rio (-1), [Nenhum,Tronco]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])]) 54
)) 
todos = TestList [teste1,teste2,teste3,teste4,teste5,teste6,teste7,teste8,teste9,teste10,teste11,teste12,teste13,teste14,teste15]


teste16 = TestCase (assertEqual ""  ())
teste17 = TestCase (assertEqual ""  ())
teste18 = TestCase (assertEqual ""  ())
teste19 = TestCase (assertEqual ""  ())
teste20 = TestCase (assertEqual ""  ())
teste21 = TestCase (assertEqual ""  ())
teste22 = TestCase (assertEqual ""  ())
teste23 = TestCase (assertEqual ""  ())
teste24 = TestCase (assertEqual ""  ())
teste25 = TestCase (assertEqual ""  ())
todos2 = TestList [teste16,teste17,teste18,teste19,teste20,teste21,teste22,teste23,teste24,teste25]



teste26 = TestCase (assertEqual ""  ())
teste27 = TestCase (assertEqual ""  ())
teste28 = TestCase (assertEqual ""  ())
teste29 = TestCase (assertEqual ""  ())
teste30 = TestCase (assertEqual ""  ())
teste31 = TestCase (assertEqual ""  ())
todos3 = TestCase [teste26,teste27,teste28,teste29,test30,teste31]

teste32 = TestCase (assertEqual ""  ())
teste33 = TestCase (assertEqual ""  ())
teste34 = TestCase (assertEqual ""  ())
teste35 = TestCase (assertEqual ""  ())
teste36 = TestCase (assertEqual ""  ())
teste37 = TestCase (assertEqual ""  ())
todos4 = [teste32,teste33,teste34,teste35,teste36,teste37]



teste38 = TestCase (assertEqual ""  ())
teste39 = TestCase (assertEqual ""  ())
teste40 = TestCase (assertEqual ""  ())
teste41 = TestCase (assertEqual ""  ())
teste42 = TestCase (assertEqual ""  ())
teste43 = TestCase (assertEqual ""  ())
teste44 = TestCase (assertEqual ""  ())
teste45 = TestCase (assertEqual ""  ())
teste46 = TestCase (assertEqual ""  ())
todos5 =[teste38,teste39,teste40,teste41,teste42,teste43,teste44,teste45,teste46]



teste47 = TestCase (assertEqual ""  ())
teste48 = TestCase (assertEqual ""  ())
teste49 = TestCase (assertEqual ""  ())
teste50 = TestCase (assertEqual ""  ())
todos6 = [teste47,teste48,teste49,teste50]

