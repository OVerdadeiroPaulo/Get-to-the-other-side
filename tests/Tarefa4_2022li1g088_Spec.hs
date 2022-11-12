module Tarefa4_2022li1g088_Spec where

import LI12223
import Tarefa4_2022li1g088
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Afoga" ~: True ~=? (jogoTerminou jogoAfoga),"Atropela" ~: True ~=? (jogoTerminou jogoAtropela),"Sai pela direita" ~: True ~=? (jogoTerminou jogosaidomapaXmaior),"Sai pela esquerda" ~: True ~=? (jogoTerminou jogosaidomapaXmenor),"Sai por Baixo" ~: True ~=? (jogoTerminou jogosaidomapaYmaior),"Sai por Cima" ~: True ~=? (jogoTerminou jogosaidomapaYmenor),"Naotermina" ~: False ~=? (jogoTerminou jogonaoterminou)]

