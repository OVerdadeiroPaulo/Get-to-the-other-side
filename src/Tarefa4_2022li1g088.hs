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
  | a < 0 || a > (l-1) = True
  | b < 0 || b >  (length lis-1) = True
  | inicionovo4 terr == "Rio" && veobstaculonacoordenada mapa (a,b) == Nenhum = True
  | inicionovo4 terr == "Rel" = False
  | otherwise = False
{-|auxiliar para comaparar terrenos-}
inicionovo4 :: Terreno -> String
inicionovo4 (Rio vel) = "Rio"
inicionovo4 (Estrada  vel) = "Est" 
inicionovo4 (Relva) =  "Rel"




mapaRioCai = Mapa 1 ([(Rio 2, [Tronco]),(Rio 2, [Nenhum])])
mapaCarroAtropela = Mapa 1 ([(Estrada  2, [Nenhum]),(Estrada  2, [Carro])])
mapaSaiDoMapa = Mapa 1 ([(Relva, [Nenhum])])
jogonaoterminou = Jogo(Jogador (0,0)) mapaSaiDoMapa
jogoAfoga = Jogo (Jogador (0,1)) mapaRioCai
jogoAtropela = Jogo (Jogador (0,1)) mapaCarroAtropela
jogosaidomapaYmaior= Jogo (Jogador (0,1)) mapaSaiDoMapa
jogosaidomapaYmenor= Jogo (Jogador (0,(-1))) mapaSaiDoMapa
jogosaidomapaXmaior= Jogo (Jogador (1,0)) mapaSaiDoMapa
jogosaidomapaXmenor= Jogo (Jogador ((-1),0)) mapaSaiDoMapa
