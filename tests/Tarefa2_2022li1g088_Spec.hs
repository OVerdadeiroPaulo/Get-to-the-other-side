module Tarefa2_2022li1g088_Spec where

import LI12223
import Tarefa2_2022li1g088
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Teste 1" ~: 1 ~=? 1]