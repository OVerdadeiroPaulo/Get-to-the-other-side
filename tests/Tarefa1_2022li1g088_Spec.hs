module Tarefa1_2022li1g088_Spec where

import LI12223
import Tarefa1_2022li1g088
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["falsoNaVelocidadeOpostadosRios " ~: False ~=? (mapaValido falsoNaVelocidadeOpostadosRios),"falsoDevidoAoTipoDeObstaculos " ~: False ~=? (mapaValido falsoDevidoAoTipoDeObstaculos),"falsoemTerrenosContiguosRios " ~: False ~=? (mapaValido falsoemTerrenosContiguosRios),"falsoemTerrenosContiguosEstradas" ~: False ~=? (mapaValido falsoemTerrenosContiguosEstradas),"falsoemTerrenosContiguosRelva " ~: False ~=? (mapaValido falsoemTerrenosContiguosRelva),"falsoNoComprimentodaLinha " ~: False ~=? (mapaValido falsoNoComprimentodaLinha),"falsonosNenhums " ~: False ~=? (mapaValido falsonosNenhums),"falsoEmComprimentodeObsCarro " ~: False ~=? (mapaValido falsoEmComprimentodeObsCarro),"falsoEmComprimentodeObsTronco " ~: False ~=? (mapaValido falsoEmComprimentodeObsTronco),"verdadeiroemtodos" ~: True ~=? (mapaValido verdadeiroemtodos)]
