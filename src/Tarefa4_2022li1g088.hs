{- |
Module      : Tarefa4_2022li1g088
Description : Determinar se o jogo terminou
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g088 where
import Tarefa3_2022li1g088
import LI12223
{-|Funcao que nos diz que o jogo terminou devolvendo True quando alguma das seguinte condiçoes é cumprida :(o Jogador sai dos limites do mapa,o Jogador ocupa as mesmas coordenadas que um Carro, ou o Jogador está num Rio na agua) -}
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (a,b)) mapa@(Mapa l (lis@((terr, obs):xs)))) 
  | veobstaculonacoordenada mapa (a,b) == Carro = True
  | a < 0 || a > l = True
  | b < 0 || b >  length lis = True
  | inicionovo terr == "Rio" && veobstaculonacoordenada mapa (a,b) == Nenhum = True
  | otherwise = False
{-|auxiliar para comaparar terrenos-}
inicionovo :: Terreno -> String
inicionovo (Rio vel) = "Rio"
inicionovo (Estrada  vel) = "Est" 
inicionovo Relva =  "Rel"