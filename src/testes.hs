module CodTest where 
import Tarefa1_2022li1g088 
import Tarefa2_2022li1g088 
import Tarefa3_2022li1g088 
import Tarefa4_2022li1g088 
import LI12223
import LI12223 (Direcao(Direita), Jogada (Parado))



f =  animaJogo jogo Parado
  where mapa = Mapa 3 [(Estrada 2, [Carro, Nenhum, Nenhum])]
        jogador = Jogador (1, 0)
        jogo = Jogo jogador mapa
g = animaJogo jogo (Move Esquerda)
  where mapa = Mapa 3 [(Estrada 2, [Carro, Nenhum, Nenhum])]
        jogador = Jogador (1, 0)
        jogo = Jogo jogador mapa

h=  animaJogo jogo Parado
  where mapa2 = Mapa 6 [(Estrada 3, [Carro, n, Carro, n, n, n])]
        jogador = Jogador (3, 0)
        jogo = Jogo jogador mapa2
        n= Nenhum
ve (Mapa _ [(_, (o:bs))]) x = reverse (drop (x+1) ( (o:bs++o:bs)))
mapa3 = Mapa 6 [(Estrada 3, [Carro, Nenhum, Carro, Nenhum, Nenhum, Nenhum])]
parviwe (o:bs) x= (drop (x)(o:bs++o:bs),reverse $ (o:bs) ++ take (x+1)(o:bs))

rio = animaJogo jogo (Move Cima)
  where mapa = Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]
        jogador = Jogador (1, 1)
        jogo = Jogo jogador mapa
rao = animaJogo jogo (Move Direita)
  where mapa = Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]
        jogador = Jogador (1, 1)
        jogo = Jogo jogador mapa

reo = animaJogo jogo (Parado)
  where mapa = Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]
        jogador = Jogador (2, 1)
        jogo = Jogo jogador mapa