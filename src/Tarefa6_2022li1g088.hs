{- |
Module      : Tarefa6_2022li1g088
Description : Funcoes do Gloss relacionadas a interacao do user e a parte grafica do programa
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}


module Main where

import LI12223
import Tarefa1_2022li1g088
import Tarefa2_2022li1g088
import Tarefa3_2022li1g088
import Tarefa4_2022li1g088
import Tarefa5_2022li1g088
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Mundo = (Paginas, Jogo, Imagens, Float)

type Imagens = [Picture]

{-| Menu que aparece quando entras no jogo -}
data MenuPrincipal = Jogar -- ^ Opcao para ir directamente ao jogo no nivel Facil ja predifinido 
                   | Dificuldades_1 -- ^ Opcao para ir ao Menu das dificuldades de jogo
                   | Instrucoes_1 -- ^ Opcao para ver as instrucoes e objectivos do jogo 
                   | Sair_1 -- ^ Opcao para sair do jogo 
                  deriving (Eq)

{-| Menu onde escolhes o nivel de dificuldade no jogo -}
data Dificuldade = Facil -- ^ Opcao para jogar em um nivel de dificuldade facil
                 | Media -- ^ Opcao para jogar em um nivel de dificuldade media 
                 | Dificil -- ^ Opcao para jogar no nivel de dificuldade mais dificil 
                 | Menu1 -- ^ opcao para voltar ao menu principal
                deriving (Eq) 

{-| Menu de Pausa, onde o jogo fica parado e o jogador pode escolher continuar o jogo ou voltar ao menu principal -}
data Pausa = Continuar_1 -- ^ Opcao para continuar o jogo pendente  
           | Menu_2 -- ^ Opcao para voltar ao menu principal

{-| Menu que aparece quando perdes o jogo -}
data MenuMorte = Reniciar -- ^ Opcao para reniciar o jogo na mesma dificuldade
               | MudarDificuldade -- ^ Opcao onde podes mudar a dificuldade do jogo 
               | Menu_3 -- ^ Opcao para Voltar ao menu principal

{-| Menu Principal quando o jogador tem um jogo pendente -}
data MenuPausa = Continuar_2 -- ^ Opcao para continuar o jogo pendente 
               | NovoJogo -- ^ Opcao para comecar um novo jogo e esquecer o pendente 
               | Dificuldades_2 -- ^ Opcao para entrar no menu das dificuldades e abrir um jogo com uma  nova dificuldade
               | Instrucoes_2 -- ^ Opcao para rever as instrucoes do jogo 
               | Sair_2 -- ^ Opcao para sair e encerrar o jogo 

{-| Menu de todas as paginas onde podemos nos encontrar no jogo, tendo em conta pausas de jogos ja a decorrer -}
data Paginas = PaginaPrincipal MenuPrincipal -- ^ A pagina principal mostranos opcoes do menu principal logo que abrimos o jogo
             | PaginaPerdeuJogo MenuMorte Dificuldade-- ^ A pagina perdeu jogo mostranos opcoes do menu morte
             | PaginaPausa Pausa Dificuldade -- ^ A pagina pausa mostranos as opcoes do menu pausa 
             | PaginaDificuldade Dificuldade Bool Dificuldade -- ^ A pagina dificuldades mostra opcoes de dificudade, tendo em conta se o menu anterior e um menu pausa ou principal
             | PaginaInstrucoes Bool Dificuldade-- ^ Apresenta as instrucoes 
             | PaginaMenuPausa MenuPausa Dificuldade -- ^ A pagina menu pausa mostranos as opcoes do menu pausa  
             | PaginaJogar Dificuldade  -- ^ A pagina jogar mostra que o jogador esta a jogar 


{-| A funcao 'estdoInicial' guarda o estado inicial de jogo, a primeira coisa que sera apresentada ao user, quando abrir o programa.

== Codigo:
@
estadoInicial :: Imagens -> Float -> Jogada -> Mundo 
estadoInicial imagens tempo jogada = (PaginaJogar, jogo1, imagens,tempo, jogada)
@  
-}

estadoInicial :: Imagens -> Float -> Mundo 
estadoInicial imagens tempo = (PaginaJogar Facil, jogo1, imagens,tempo)



{-| A funcao 'desenhaMundo' devolve a picture do 'Mundo' no ecra e nos permite ver o resultado das nossas interacoes com o programa

==codigo:
@

@
-}
desenhaMundo :: Mundo -> Picture
--PaginaPrincipal 
desenhaMundo (PaginaPrincipal Jogar, jogo, imagens, tempo) = fundoAnimado1 {--Pictures [Scale 1.0 1.0 (imagens !! 4)] --}
            where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
desenhaMundo (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo) = fundoAnimado2    {-Picture [Scale 1.0 1.0 (imagens !! 4)]-} 
            where fundoAnimado2 = imagefundo2 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
desenhaMundo (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo) = fundoAnimado3   {-Pictures [Scale 1.0 1.0 (imagens !! 4)]-} 
            where fundoAnimado3 = imagefundo3 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
desenhaMundo (PaginaPrincipal Sair_1, jogo, imagens, tempo) = fundoAnimado4  {--Pictures [Scale 1.0 1.0 (imagens !! 4)]--}
            where fundoAnimado4 = imagefundo4 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
--PaginaPerdeuJogo
desenhaMundo (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens,tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
--PaginaPausa
desenhaMundo (PaginaPausa Continuar_1 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaPausa Menu_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
--PaginaDificuldade
desenhaMundo (PaginaDificuldade Facil b d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaDificuldade Media b d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaDificuldade Dificil b d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaDificuldade Menu1 b d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
--paginaInstrucoes 
desenhaMundo (PaginaInstrucoes b d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
--PaginaMenuPaus
desenhaMundo (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
--PaginaJogar 
desenhaMundo (PaginaJogar d, jogo, imagens, tempo) | d == Facil = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world28 
                                                   | d == Media = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world29
                                                   | d == Dificil = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world30
 where 
     world28 = desenhaTerrenos1 ++ desenhaObstaculos1 ++ [tempoDeJogo1] ++[scoore1] ++ [desenhajogador1] -- ^ Mundo da dificuldade Facil 
     desenhaTerrenos1 = criarTerreno1 p o (getLargura(getMapa (PaginaJogar Facil, jogo, imagens, tempo))) (getTerreno(getMapa (PaginaJogar Facil, jogo, imagens, tempo))) imagens
     desenhaObstaculos1 = criarObstaculos1 p o (getTerreno(getMapa (PaginaJogar Facil, jogo, imagens, tempo))) imagens 
     desenhajogador1 = criarJogador1 (getJogador (PaginaJogar Facil, jogo, imagens, tempo)) (getTempo (PaginaJogar Facil, jogo, imagens, tempo)) imagens
     tempoDeJogo1 = mostrarTempo (getTempo(PaginaJogar Facil, jogo, imagens, tempo))
     scoore1 = mostrarScoore (getTempo(PaginaJogar Facil, jogo, imagens, tempo)) 
    
     world29 = desenhaTerrenos2 ++ desenhaObstaculos2 ++ [tempoDeJogo2] ++ [scoore2] ++ [desenhajogador2] -- ^ Mundo da dificuldade Media
     desenhaTerrenos2 = criarTerreno2 p o (getLargura(getMapa (PaginaJogar Media, jogo, imagens, tempo))) (getTerreno(getMapa (PaginaJogar Media, jogo, imagens, tempo))) imagens
     desenhaObstaculos2 = criarObstaculos2 p o (getTerreno(getMapa (PaginaJogar Media, jogo, imagens, tempo))) imagens 
     desenhajogador2 = criarJogador2 (getJogador (PaginaJogar Media, jogo, imagens, tempo)) (getTempo (PaginaJogar Media, jogo, imagens, tempo)) imagens
     tempoDeJogo2 = mostrarTempo (getTempo(PaginaJogar Media, jogo, imagens, tempo))
     scoore2 = mostrarScoore (getTempo(PaginaJogar Media, jogo, imagens, tempo))
    
     world30 = desenhaTerrenos3 ++ desenhaObstaculos3 ++ [tempoDeJogo3] ++ [scoore3] ++ [desenhajogador3] -- ^ Mundo da dificuldade Dificil
     desenhaTerrenos3 = criarTerreno3 p o (getLargura(getMapa (PaginaJogar Dificil, jogo, imagens, tempo))) (getTerreno(getMapa (PaginaJogar Dificil, jogo, imagens, tempo))) imagens
     desenhaObstaculos3 = criarObstaculos3 p o (getTerreno(getMapa (PaginaJogar Dificil, jogo, imagens, tempo))) imagens 
     desenhajogador3 = criarJogador3 (getJogador (PaginaJogar Dificil, jogo, imagens, tempo)) (getTempo (PaginaJogar Dificil, jogo, imagens, tempo)) imagens
     tempoDeJogo3 = mostrarTempo (getTempo(PaginaJogar Dificil, jogo, imagens, tempo))
     scoore3 = mostrarScoore (getTempo(PaginaJogar Dificil, jogo, imagens, tempo))

{-| A funcao 'mostraTempo' mostra o tempo a ser registrado no jogo a passar

== Codigo:
@
mostrarTempo :: Float -> Picture
mostrarTempo t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)
@
-}

mostrarTempo :: Float -> Picture
mostrarTempo t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)

{-| A funcao 'mostraTempo' mostra o tempo a ser registrado no jogo a passar

== Codigo:
@
mostrarScoore :: Float -> Picture
mostrarScoore t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)
@
-}

mostrarScoore :: Float -> Picture
mostrarScoore t =  Translate 630 (-160) $ scale 0.2 0.2 $ Text (show $ round (t*100))

{-| A funcao 'imageFundo1' faz alternar as imagens do background do jogo

== Codigo:
@
imagefundo1 :: Float -> Imagens -> Picture 
imagefundo1 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                     | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                     | otherwise = (imagens !! 12)
@
-}

imagefundo1 :: Float -> Imagens -> Picture 
imagefundo1 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                      | otherwise = (imagens !! 12)

{-| A funcao 'imageFundo2' faz alternar as imagens do background do jogo

== Codigo:
@
imagefundo2 :: Float -> Imagens -> Picture 
imagefundo2 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                      | otherwise = (imagens !! 12)
@
-}

imagefundo2 :: Float -> Imagens -> Picture 
imagefundo2 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                      | otherwise = (imagens !! 12)

{-| A funcao 'imageFundo3' faz alternar as imagens do background do jogo

== Codigo:
@
imagefundo3 :: Float -> Imagens -> Picture 
imagefundo3 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                      | otherwise = (imagens !! 12)
@
-}

imagefundo3 :: Float -> Imagens -> Picture 
imagefundo3 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                      | otherwise = (imagens !! 12)

{-| A funcao 'imageFundo4' faz alternar as imagens do background do jogo

== Codigo:
@
imagefundo4 :: Float -> Imagens -> Picture 
imagefundo4 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                      | otherwise = (imagens !! 12)
@
-}

imagefundo4 :: Float -> Imagens -> Picture 
imagefundo4 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                      | otherwise = (imagens !! 12)                      

{-| A funcao 'getImage' busca a imagem no 'Mundo'

== Codigo: 
@
getImagens :: Mundo -> Imagens
getImagens (_, _, i, _) = i
@
-}

getImagens :: Mundo -> Imagens
getImagens (_, _, i, _) = i
                              
{-| A funcao auxiliar 'getTempo' busca o tempo no 'Mundo'
@
getTempo :: Mundo -> Float 
getTempo (_, _, _, tempo) = tempo
@
-}

getTempo :: Mundo -> Float 
getTempo (_, _, _, tempo) = tempo 

{-| A funcao 'getMapa' extrai o Mapa do 'Jogo' que esta dentro do 'Mundo' 

== Codigo:
@
getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _, _) = m
@ 
-}

getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _, _) = m 

{-| A  funcao 'getLargura' busca a largura no 'Mapa'

== Codigo:
@
getLargura :: Mapa -> Int 
getLargura (Mapa l ((te,obs):xs)) = l
@
-}

getLargura :: Mapa -> Int 
getLargura (Mapa l ((te,obs):xs)) = l

{-| A funcao 'getTerreno' busca o '[(Terreno,[Obstaculo])]' no 'Mapa'

== Codigo:
@
getTerreno :: Mapa -> [(Terreno,[Obstaculo])] 
getTerreno (Mapa l ((te,obs):xs)) = ((te,obs):xs)
@
-}

getTerreno :: Mapa -> [(Terreno,[Obstaculo])] 
getTerreno (Mapa l ((te,obs):xs)) = ((te,obs):xs)


{-| A Funcao 'getJogador' extrai o 'Jogador' do 'Mundo'

== Codigo:
@
getJogador :: Mundo -> Jogador
getJogador (_, Jogo j m, _, _) = j
@
-}

getJogador :: Mundo -> Jogador
getJogador (_, Jogo j m, _, _) = j

{-| O 'p' guarda o valor do x onde o Mapa vai comecar

== Codigo:
@
p :: Float
p = 0.0
@
-}

p :: Float 
p = 0.0

{-|O 'o' guarda o Valor do y onde o Mapa vai comecar

== Codigo:
@
o :: Float
o = 0.0
@
-}

o :: Float 
o = 0.0

{-| O 'lado' guarda o valor do lado da imagem, usado para contruir as figuras seguintes uma apos a outra e usado para controir as linhas uma assima da outra sem se sobreporem.

== Codigo:
@
lado :: Float
lado = 60.0 
@
-}

lado :: Float 
lado = 60.0 


{-| Funcao 'desenhaLinha' e a Funcao auxiliar da 'criarTerreno' que desenha uma linha do mapa da dificuldade 'Facil'

== Codigo: 
@
desenhaLinhaTer1 :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinhaTer1 x y 0 te imagens = []
desenhaLinhaTer1 x y la te imagens = terreno : linha 
                            where terreno = desenhaTer1 x y te imagens
                                  linha = desenhaLinhaTer1 (x + lado) y (la-1) te imagens
desenhaLinhaTer1 _ _ _ _ _ = []
@
-}

desenhaLinhaTer1 :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinhaTer1 x y 0 te imagens = []
desenhaLinhaTer1 x y la te imagens = terreno : linha 
                            where terreno = desenhaTer1 x y te imagens
                                  linha = desenhaLinhaTer1 (x + lado) y (la-1) te imagens
desenhaLinhaTer1 _ _ _ _ _ = []

{-| Funcao 'desenhaLinha' e a Funcao auxiliar da 'criarTerreno' que desenha uma linha do mapa da dificuldade 'Media'

== Codigo: 
@
desenhaLinhaTer2 :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinhaTer2 x y 0 te imagens = []
desenhaLinhaTer2 x y la te imagens = terreno : linha 
                            where terreno = desenhaTer2 x y te imagens
                                  linha = desenhaLinhaTer2 (x + lado) y (la-1) te imagens
desenhaLinhaTer2 _ _ _ _ _ = []
@
-}

desenhaLinhaTer2 :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinhaTer2 x y 0 te imagens = []
desenhaLinhaTer2 x y la te imagens = terreno : linha 
                            where terreno = desenhaTer2 x y te imagens
                                  linha = desenhaLinhaTer2 (x + lado) y (la-1) te imagens
desenhaLinhaTer2 _ _ _ _ _ = []

{-| Funcao 'desenhaLinha' e a Funcao auxiliar da 'criarTerreno' que desenha uma linha do mapa da dificuldade 'Dificil' 

== Codigo: 
@
desenhaLinhaTer3 :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinhaTer3 x y 0 te imagens = []
desenhaLinhaTer3 x y la te imagens = terreno : linha 
                            where terreno = desenhaTer3 x y te imagens
                                  linha = desenhaLinhaTer3 (x + lado) y (la-1) te imagens
desenhaLinhaTer3 _ _ _ _ _ = []
@
-}

desenhaLinhaTer3 :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinhaTer3 x y 0 te imagens = []
desenhaLinhaTer3 x y la te imagens = terreno : linha 
                            where terreno = desenhaTer3 x y te imagens
                                  linha = desenhaLinhaTer3 (x + lado) y (la-1) te imagens
desenhaLinhaTer3 _ _ _ _ _ = []


{-| Funcao 'desenhaLinhaObs' e a funcao auxiliar da 'criarObstaculos' que desenha a linha de obstaculos no mapa da dificuldade  'Facil'

==codigo"
@
desenhaLinhaObs1 :: Float -> Float -> [Obstaculo] -> Imagens -> [Picture]
desenhaLinhaObs1 x y [] imagens = []
desenhaLinhaObs1 x y (z:zs) imagens = obstaculos : linha
                                 where obstaculos = desenhaObs1 x y z imagens 
                                       linha = desenhaLinhaObs1 (x + lado) y zs imagens
desenhaLinhaObs1 _ _ _ _ = []
@
-}

desenhaLinhaObs1 :: Float -> Float -> [Obstaculo] -> Imagens -> [Picture]
desenhaLinhaObs1 x y [] imagens = []
desenhaLinhaObs1 x y (z:zs) imagens = obstaculos : linha
                                 where obstaculos = desenhaObs1 x y z imagens 
                                       linha = desenhaLinhaObs1 (x + lado) y zs imagens
desenhaLinhaObs1 _ _ _ _ = []

{-| Funcao 'desenhaLinhaObs' e a funcao auxiliar da 'criarObstaculos' que desenha a linha de obstaculos no mapa da dificuldade  'Media'

==codigo"
@
desenhaLinhaObs2 :: Float -> Float -> [Obstaculo] -> Imagens -> [Picture]
desenhaLinhaObs2 x y [] imagens = []
desenhaLinhaObs2 x y (z:zs) imagens = obstaculos : linha
                                 where obstaculos = desenhaObs2 x y z imagens 
                                       linha = desenhaLinhaObs2 (x + lado) y zs imagens
desenhaLinhaObs2 _ _ _ _ = []
@
-}

desenhaLinhaObs2 :: Float -> Float -> [Obstaculo] -> Imagens -> [Picture]
desenhaLinhaObs2 x y [] imagens = []
desenhaLinhaObs2 x y (z:zs) imagens = obstaculos : linha
                                 where obstaculos = desenhaObs2 x y z imagens 
                                       linha = desenhaLinhaObs2 (x + lado) y zs imagens
desenhaLinhaObs2 _ _ _ _ = []

{-| Funcao 'desenhaLinhaObs' e a funcao auxiliar da 'criarObstaculos' que desenha a linha de obstaculos no mapa da dificuldade  'Dificil'

==codigo"
@
desenhaLinhaObs3 :: Float -> Float -> [Obstaculo] -> Imagens -> [Picture]
desenhaLinhaObs3 x y [] imagens = []
desenhaLinhaObs3 x y (z:zs) imagens = obstaculos : linha
                                 where obstaculos = desenhaObs3 x y z imagens 
                                       linha = desenhaLinhaObs3 (x + lado) y zs imagens
desenhaLinhaObs3 _ _ _ _ = []
@
-}

desenhaLinhaObs3 :: Float -> Float -> [Obstaculo] -> Imagens -> [Picture]
desenhaLinhaObs3 x y [] imagens = []
desenhaLinhaObs3 x y (z:zs) imagens = obstaculos : linha
                                 where obstaculos = desenhaObs3 x y z imagens 
                                       linha = desenhaLinhaObs3 (x + lado) y zs imagens
desenhaLinhaObs3 _ _ _ _ = []


{-| Funcao 'desenhaTer1' e a auxiliar da 'desenhaLinhaTer1' que com o valor x e y esta funcao cria uma picture com a imagem 'Terreno' correspondente da dificuldade 'Facil' 

== Codigo:
@
desenhaTer1 :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer1 x y terreno imagens = Translate x y image 
                       where image = renderTer1 terreno imagens 
@
-}

desenhaTer1 :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer1 x y terreno imagens = Translate x y image 
                       where image = renderTer1 terreno imagens

{-| Funcao 'desenhaTer2' e a auxiliar da 'desenhaLinhaTer2' que com o valor x e y esta funcao cria uma picture com a imagem 'Terreno' correspondente da dificuldade 'Media' 

== Codigo:
@
desenhaTer2 :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer2 x y terreno imagens = Translate x y image 
                       where image = renderTer2 terreno imagens 
@
-}

desenhaTer2 :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer2 x y terreno imagens = Translate x y image 
                       where image = renderTer2 terreno imagens 

{-| Funcao 'desenhaTer3' e a auxiliar da 'desenhaLinhaTer3' que com o valor x e y esta funcao cria uma picture com a imagem 'Terreno' correspondente da dificuldade 'Dificil' 

== Codigo:
@
desenhaTer3 :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer3 x y terreno imagens = Translate x y image 
                       where image = renderTer3 terreno imagens 
@
-}

desenhaTer3 :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer3 x y terreno imagens = Translate x y image 
                       where image = renderTer3 terreno imagens


{-| Funcao 'desenhaObs1' e a auxiliar da 'desenhaLinhaObs1' que com o valor x e y ela cria uma picture com a imagem do 'Obstaculo' correspondente da dificuldade 'Facil'

== Codigo:
@
desenhaObs :: Float -> Float -> Obstaculo -> Imagens -> Picture
desenhaObs x y obstaculo imagens = Translate x y image
                         where image = render2 obstaculo imagens
@ 
-}

desenhaObs1 :: Float -> Float -> Obstaculo -> Imagens -> Picture
desenhaObs1 x y obstaculo imagens = Translate x y image
                         where image = renderObs1 obstaculo imagens

{-| Funcao 'desenhaObs2' e a auxiliar da 'desenhaLinhaObs2' que com o valor x e y ela cria uma picture com a imagem do 'Obstaculo' correspondente da dificuldade 'Media'

== Codigo:
@
desenhaObs2 :: Float -> Float -> Obstaculo -> Imagens -> Picture
desenhaObs2 x y obstaculo imagens = Translate x y image
                         where image = render2 obstaculo imagens
@ 
-}

desenhaObs2 :: Float -> Float -> Obstaculo -> Imagens -> Picture
desenhaObs2 x y obstaculo imagens = Translate x y image
                         where image = renderObs2 obstaculo imagens

{-| Funcao 'desenhaObs3' e a auxiliar da 'desenhaLinhaObs2' que com o valor x e y ela cria uma picture com a imagem do 'Obstaculo' correspondente da dificuldade 'Dificil'

== Codigo:
@
desenhaObs3 :: Float -> Float -> Obstaculo -> Imagens -> Picture
desenhaObs3 x y obstaculo imagens = Translate x y image
                         where image = render3 obstaculo imagens
@ 
-}

desenhaObs3 :: Float -> Float -> Obstaculo -> Imagens -> Picture
desenhaObs3 x y obstaculo imagens = Translate x y image
                         where image = renderObs3 obstaculo imagens


{-| Funcao 'renderTer1' e a funcao auxiliar da funcao 'desenhaTer1' funcao junta cada 'Terreno' a uma 'Imagem' ja definida para a dificuldade 'Facil' 

== Codigo: 
@
renderTer1 :: Terreno -> Imagens -> Picture 
renderTer1 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 13)
 | inicionovo terreno == "Rio" = (imagens !! 15)
 | inicionovo terreno == "Est" = (imagens !! 17)
@
-}

renderTer1 :: Terreno -> Imagens -> Picture 
renderTer1 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 13)
 | inicionovo terreno == "Rio" = (imagens !! 15)
 | inicionovo terreno == "Est" = (imagens !! 17)
 
{-| Funcao 'renderTer2' e a funcao auxiliar da funcao 'desenhaTer2' funcao junta cada 'Terreno' a uma 'Imagem' ja definida para a dificuldade 'Media' 

== Codigo: 
@
renderTer2 :: Terreno -> Imagens -> Picture 
renderTer2 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 13)
 | inicionovo terreno == "Rio" = (imagens !! 15)
 | inicionovo terreno == "Est" = (imagens !! 17)
@
-}

renderTer2 :: Terreno -> Imagens -> Picture 
renderTer2 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 13)
 | inicionovo terreno == "Rio" = (imagens !! 15)
 | inicionovo terreno == "Est" = (imagens !! 17)

{-| Funcao 'renderTer3' e a funcao auxiliar da funcao 'desenhaTer3' funcao junta cada 'Terreno' a uma 'Imagem' ja definida para a dificuldade 'Dificil' 

== Codigo: 
@
renderTer3 :: Terreno -> Imagens -> Picture 
renderTer3 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 13)
 | inicionovo terreno == "Rio" = (imagens !! 15)
 | inicionovo terreno == "Est" = (imagens !! 17)
@
-}

renderTer3 :: Terreno -> Imagens -> Picture 
renderTer3 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 13)
 | inicionovo terreno == "Rio" = (imagens !! 15)
 | inicionovo terreno == "Est" = (imagens !! 17)

{-| Funcao 'renderObs1' e a funcao auxiliar da funcao 'desenhaObs1', esta funcao junta cada 'Obstaculo' a uma 'Imagem' ja definida para a dificuldade 'Facil'

== Codigo:
@
renderObs1 :: Obstaculo -> Imagens -> Picture 
renderObs1 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 8)
 | obstaculo == Tronco = (imagens !! 16)
 | obstaculo == Arvore = (imagens !! 14)
 | obstaculo == Carro = (imagens !! 18)
@
-}

renderObs1 :: Obstaculo -> Imagens -> Picture 
renderObs1 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 8)
 | obstaculo == Tronco = (imagens !! 16)
 | obstaculo == Arvore = (imagens !! 14)
 | obstaculo == Carro = (imagens !! 18)

{-| Funcao 'renderObs2' e a funcao auxiliar da funcao 'desenhaObs2', esta funcao junta cada 'Obstaculo' a uma 'Imagem' ja definida para a dificuldade 'Media'

== Codigo:
@
renderObs2 :: Obstaculo -> Imagens -> Picture 
renderObs2 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 8)
 | obstaculo == Tronco = (imagens !! 16)
 | obstaculo == Arvore = (imagens !! 14)
 | obstaculo == Carro = (imagens !! 18)
@
-}

renderObs2 :: Obstaculo -> Imagens -> Picture 
renderObs2 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 8)
 | obstaculo == Tronco = (imagens !! 16)
 | obstaculo == Arvore = (imagens !! 14)
 | obstaculo == Carro = (imagens !! 18)

{-| Funcao 'renderObs3' e a funcao auxiliar da funcao 'desenhaObs3', esta funcao junta cada 'Obstaculo' a uma 'Imagem' ja definida para a dificuldade 'Media'

== Codigo:
@
renderObs3 :: Obstaculo -> Imagens -> Picture 
renderObs3 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 8)
 | obstaculo == Tronco = (imagens !! 16)
 | obstaculo == Arvore = (imagens !! 14)
 | obstaculo == Carro = (imagens !! 18)
@
-}

renderObs3 :: Obstaculo -> Imagens -> Picture 
renderObs3 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 8)
 | obstaculo == Tronco = (imagens !! 16)
 | obstaculo == Arvore = (imagens !! 14)
 | obstaculo == Carro = (imagens !! 18)

{-| Funcao 'criarTerreno1' esta funcao cria o Mapa usando o 'desenhalinhaTer1' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Terrenos' desejados para a dificuldade 'Facil'

== Codigo:  
@
criarTerreno1 :: Float -> Float -> Int -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] 
criarTerreno1 x y la ((te,obs):xs) imagens = line ++ linhaseguinte 
                              where line = desenhaLinhaTer1 x y la te imagens 
                                    linhaseguinte = criarTerreno1 x (y - lado) la (xs) imagens 
criarTerreno1 _ _ _ _ _ = []
@ 
-}

criarTerreno1 :: Float -> Float -> Int -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] 
criarTerreno1 x y la ((te,obs):xs) imagens = line ++ linhaseguinte 
                              where line = desenhaLinhaTer1 x y la te imagens 
                                    linhaseguinte = criarTerreno1 x (y - lado) la (xs) imagens 
criarTerreno1 _ _ _ _ _ = []

{-| Funcao 'criarTerreno2' esta funcao cria o Mapa usando o 'desenhalinhaTer2' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Terrenos' desejados para a dificuldade 'Media'

== Codigo:  
@
criarTerreno2 :: Float -> Float -> Int -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] 
criarTerreno2 x y la ((te,obs):xs) imagens = line ++ linhaseguinte 
                              where line = desenhaLinhaTer2 x y la te imagens 
                                    linhaseguinte = criarTerreno2 x (y - lado) la (xs) imagens 
criarTerreno2 _ _ _ _ _ = []
@ 
-}

criarTerreno2 :: Float -> Float -> Int -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] 
criarTerreno2 x y la ((te,obs):xs) imagens = line ++ linhaseguinte 
                              where line = desenhaLinhaTer2 x y la te imagens 
                                    linhaseguinte = criarTerreno2 x (y - lado) la (xs) imagens 
criarTerreno2 _ _ _ _ _ = []

{-| Funcao 'criarTerreno3' esta funcao cria o Mapa usando o 'desenhalinhaTer3' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Terrenos' desejados para a dificuldade 'Media'

== Codigo:  
@
criarTerreno3 :: Float -> Float -> Int -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] 
criarTerreno3 x y la ((te,obs):xs) imagens = line ++ linhaseguinte 
                              where line = desenhaLinhaTer3 x y la te imagens 
                                    linhaseguinte = criarTerreno3 x (y - lado) la (xs) imagens 
criarTerreno3 _ _ _ _ _ = []
@ 
-}

criarTerreno3 :: Float -> Float -> Int -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] 
criarTerreno3 x y la ((te,obs):xs) imagens = line ++ linhaseguinte 
                              where line = desenhaLinhaTer3 x y la te imagens 
                                    linhaseguinte = criarTerreno3 x (y - lado) la (xs) imagens 
criarTerreno3 _ _ _ _ _ = []

{-| Funcao 'criarObstaculos1' esta funcao cria o Mapa usando o 'desenhalinhaObs1' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Obstaculos' desejados para a dificuldade 'Facil'

== Codigo:  
@
criarObstaculos1 :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
criarObstaculos1 x y ((z,w):zs) imagens = line ++ linhaseguinte
                                 where line = desenhaLinhaObs1 x y w imagens
                                       linhaseguinte = criarObstaculos1 x (y - lado) (zs) imagens 
criarObstaculos1 _ _ _ _ = []
@
-}

criarObstaculos1 :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
criarObstaculos1 x y ((z,w):zs) imagens = line ++ linhaseguinte
                                 where line = desenhaLinhaObs1 x y w imagens
                                       linhaseguinte = criarObstaculos1 x (y - lado) (zs) imagens 
criarObstaculos1 _ _ _ _ = [] 

{-| Funcao 'criarObstaculos2' esta funcao cria o Mapa usando o 'desenhalinhaObs2' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Obstaculos' desejados para a dificuldade 'Media'

== Codigo:  
@
criarObstaculos2 :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
criarObstaculos2 x y ((z,w):zs) imagens = line ++ linhaseguinte
                                 where line = desenhaLinhaObs2 x y w imagens
                                       linhaseguinte = criarObstaculos2 x (y - lado) (zs) imagens 
criarObstaculos2 _ _ _ _ = []
@
-}

criarObstaculos2 :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
criarObstaculos2 x y ((z,w):zs) imagens = line ++ linhaseguinte
                                 where line = desenhaLinhaObs2 x y w imagens
                                       linhaseguinte = criarObstaculos2 x (y - lado) (zs) imagens 
criarObstaculos2 _ _ _ _ = [] 

{-| Funcao 'criarObstaculos3' esta funcao cria o Mapa usando o 'desenhalinhaObs3' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Obstaculos' desejados para a dificuldade 'Dificil'

== Codigo:  
@
criarObstaculos3 :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
criarObstaculos3 x y ((z,w):zs) imagens = line ++ linhaseguinte
                                 where line = desenhaLinhaObs3 x y w imagens
                                       linhaseguinte = criarObstaculos3 x (y - lado) (zs) imagens 
criarObstaculos3 _ _ _ _ = []
@
-}

criarObstaculos3 :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
criarObstaculos3 x y ((z,w):zs) imagens = line ++ linhaseguinte
                                 where line = desenhaLinhaObs3 x y w imagens
                                       linhaseguinte = criarObstaculos3 x (y - lado) (zs) imagens 
criarObstaculos3 _ _ _ _ = [] 


{-| Funcao 'criarJogador1' pega no jogador, num float e numa imagem e devolve a picture do jogador, e com a ajuda das auxiliares 'saltaX' e 'saltaY' ela tranlada o jogador sempre para o centro do novo bloco de 'Terreno' e por causa das guardas e do '(mod (round (t*1000)) 300) < 100' conseguimos alternar entre as imagens do jogador a cada 100 ms criando um jogador em movimento perpetuo, na dificuldade 'Facil'.

==codigo:
@
criarJogador :: Jogador -> Float-> Imagens -> Picture
criarJogador (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                       | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                       | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)
@ 
-}
                                                                     
criarJogador1 :: Jogador -> Float-> Imagens -> Picture
criarJogador1 (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                        | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                        | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)                         

{-| Funcao 'criarJogador2' pega no jogador, num float e numa imagem e devolve a picture do jogador, e com a ajuda das auxiliares 'saltaX' e 'saltaY' ela tranlada o jogador sempre para o centro do novo bloco de 'Terreno' e por causa das guardas e do '(mod (round (t*1000)) 300) < 100' conseguimos alternar entre as imagens do jogador criando um jogador em movimento perpetuo, na dificuldade 'Media'.

==codigo:
@
criarJogador2 :: Jogador -> Float-> Imagens -> Picture
criarJogador2 (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                       | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                       | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)  
@ 
-}
                                                                     
criarJogador2 :: Jogador -> Float-> Imagens -> Picture
criarJogador2 (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                        | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                        | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)

{-| Funcao 'criarJogador3' pega no jogador, num float e numa imagem e devolve a picture do jogador, e com a ajuda das auxiliares 'saltaX' e 'saltaY' ela tranlada o jogador sempre para o centro do novo bloco de 'Terreno' e por causa das guardas e do '(mod (round (t*1000)) 300) < 100' conseguimos alternar entre as imagens do jogador criando um jogador em movimento perpetuo, na dificuldade 'Dificil'.

==codigo:
@
criarJogador3 :: Jogador -> Float-> Imagens -> Picture
criarJogador3 (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                       | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                       | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)
@ 
-}
                                                                     
criarJogador3 :: Jogador -> Float-> Imagens -> Picture
criarJogador3 (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                        | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                        | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)


{-| Funcoes auxiliares que controlam o movimento do jogador no mapa, feitas com as medidas perfeitas para o jogador ficar sempre no centro do Mapa
event :: Event -> Mundo -> Mundo 
-- Pagina Principal 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Jogar, jogo, imagens) = (PaginaPrincipal Sair_1, jogo, imagens) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens) = (PaginaPrincipal Jogar, jogo, imagens) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens) = (PaginaPrincipal Dificuldades_1, jogo, imagens) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens) = (PaginaPrincipal Instrucoes_1, jogo, imagens) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Jogar, jogo, imagens) = (PaginaPrincipal Dificuldades_1, jogo, imagens) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens) = (PaginaPrincipal Instrucoes_1, jogo, imagens) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens) = (PaginaPrincipal Sair_1, jogo, imagens) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens) = (PaginaPrincipal Jogar, jogo, imagens) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Jogar, Jogo j m, imagens) = (PaginaJogar Facil, Jogo j m, imagens) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens) = (PaginaDificuldade Facil false Facil, joga, imagens) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens) = (PaginaInstrucoes false Facil, jogo, imagens) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens) = error "Jogo Terminou"
-- Pagina Dificuldade
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens) = (PaginaDificuldade Menu1 b d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Media b d, jogo, imagens) = (PaginaDificuldade Facil b d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens) = (PaginaDificuldade Media b d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens) = (PaginaDificuldade Dificil b d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens) = (PaginaDificuldade Media b d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Media b d, jogo, imagens) = (PaginaDificuldade Dificil b d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens) = (PaginaDificuldade Menu1 b d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens) = (PaginaDificuldade Facil b d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens) = (PaginaJogar d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Media b d, jogo, imagens) = (PaginaJogar d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens) = (PaginaJogar d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens) | b == True (PaginaMenuPausa Dificuldades_2 d, jogo, imagens)
                                                                                                 | otherwise = (PaginaPrincipal Dificuldades_1, jogo, imagens)
-- Pagina controlos  
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaInstrucoes b d, jogo, imagens) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens)
                                                                                      | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaInstrucoes b d, jogo, imagens) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens)
                                                                                   | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaInstrucoes b d, jogo, imagens) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens)
                                                                                      | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens)
-- Pagina Pausa  
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Continuar_1 d, Jogo j m, imagens) = (PaginaPausa Menu_2 d, Jogo j m, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Menu_2 d, Jogo j m, imagens) = (PaginaPausa Continuar_1 , Jogo j m, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Continuar_1 d, Jogo j m, imagens) = (PaginaPausa Menu_2, Jogo j m, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Menu_2 d, Jogo j m, imagens) = (PaginaPausa Continuar_1 , Jogo j m, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Continuar_1 d, jogo, imagens) = (PaginaJogar d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Menu_2 d, jogo, imagens) = (PaginaMenuPausa Continuar_2 d, jogo, imagens)
-- Pagina MenuPausa 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens) = (PaginaMenuPausa Sair_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens) = (PaginaMenuPausa Continuar_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens) = (PaginaMenuPausa NovoJogo d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens) = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens) = (PaginaMenuPausa Instrucoes_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens) = (PaginaMenuPausa NovoJogo d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens) = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens) = (PaginaMenuPausa Instrucoes_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens) = (PaginaMenuPausa Sair_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens) = (PaginaMenuPausa Continuar_2 d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens) = (PaginaJogar d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens) = (PaginaJogar Facil, Jogo j d, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens) = (PaginaDificuldade Facil True d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens) = (PaginaInstrucoes True d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens) = error "Terminou Jogo"
-- Pagina Perdeu jogo
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens) = (PaginaPerdeuJogo Menu_3 d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens) = (PaginaPerdeuJogo Reniciar d, jogo, imagens)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens) = (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens) = (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens) = (PaginaPerdeuJogo Menu_3 d, jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens) = (PaginaPerdeuJogo Reniciar d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens) = (PaginaJogar d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens) = (PaginaDificuldade Facil False d, jogo, imagens)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens) = (PaginaPrincipal Jogar, jogo, imagens)
-- Pagina Jogar 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Cima-}jogo, imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Baixo-}jogo, imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Esquerda-}jogo, imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Direita-}jogo, imagens)
event (EventKey (SpecialKey KeySpace) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaPausa Continuar_1 d, jogo, imagens)
event _ s = s

==codigo:
@
saltaX :: Int -> Float -- ^ Funcao auxiliar que controla o movimento no eixo do 'x'
saltaX = (+p).(*lado).realToFrac
                              
saltaY :: Int -> Float -- ^ Funcao auxiliar que controla o movimento no eixo do 'Y'
saltaY = (+o).(*(-lado)).realToFrac
@
-}

saltaX :: Int -> Float -- ^ Funcao auxiliar que controla o movimento no eixo do 'x'
saltaX = (+p).(*lado).realToFrac
                              
saltaY :: Int -> Float -- ^ Funcao auxiliar que controla o movimento no eixo do 'Y'
saltaY = (+o).(*(-lado)).realToFrac 

{-|A funcao 'event' da accao as teclas para que o user possa navegar pelo jogo e pelos diferentes 'Menus' interagindo com o teclado

==cogigo:
@
@
-}
event :: Event -> Mundo -> Mundo 
-- Pagina Principal 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Jogar, jogo, imagens, tempo) = (PaginaPrincipal Sair_1, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo) = (PaginaPrincipal Jogar, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo) = (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens, tempo) = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Jogar, jogo, imagens, tempo) = (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo) = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo) = (PaginaPrincipal Sair_1, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens, tempo) = (PaginaPrincipal Jogar, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Jogar, jogo, imagens, tempo) = (PaginaJogar Facil, jogo1, imagens, tempo) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo) = (PaginaDificuldade Facil False Facil, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo) = (PaginaInstrucoes False Facil, jogo, imagens, tempo) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens, tempo) = error "Jogo Terminou"
-- Pagina Dificuldade
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens, tempo) = (PaginaDificuldade Menu1 b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Media b d, jogo, imagens, tempo) = (PaginaDificuldade Facil b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens, tempo) = (PaginaDificuldade Media b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens, tempo) = (PaginaDificuldade Dificil b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens, tempo) = (PaginaDificuldade Media b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Media b d, jogo, imagens, tempo) = (PaginaDificuldade Dificil b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens, tempo) = (PaginaDificuldade Menu1 b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens, tempo) = (PaginaDificuldade Facil b d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens, tempo) = (PaginaJogar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Media b d, jogo, imagens, tempo) = (PaginaJogar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens, tempo) = (PaginaJogar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens, tempo) | b == True = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo)
                                                                                                    | otherwise = (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo)
-- Pagina controlos  
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaInstrucoes b d, jogo, imagens, tempo) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo)
                                                                                          | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaInstrucoes b d, jogo, imagens, tempo) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo)
                                                                                            | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaInstrucoes b d, jogo, imagens, tempo) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo)
                                                                                             | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo)
-- Pagina Pausa  
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo) = (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo) = (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo) = (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo) = (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Continuar_1 d, jogo, imagens, tempo) = (PaginaJogar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Menu_2 d, jogo, imagens, tempo) = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo)
-- Pagina MenuPausa 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo) = (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo) = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo) = (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo) = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo) = (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo) = (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo) = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo) = (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo) = (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo) = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo) = (PaginaJogar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo) = (PaginaJogar d, jogo1 , imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo) = (PaginaDificuldade Facil True d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo) = (PaginaInstrucoes True d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo) = error "Terminou Jogo"
-- Pagina Perdeu jogo
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo) = (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo) = (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo) = (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo) = (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo) = (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo) = (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo) = (PaginaJogar d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo) = (PaginaDificuldade Facil False d, jogo, imagens, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo) = (PaginaPrincipal Jogar, jogo, imagens, tempo)
-- Pagina Jogar 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo) = (PaginaJogar d, Jogo (deslocafinal j (Move Cima) m) m, imagens, tempo)           
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo) = (PaginaJogar d, Jogo (deslocafinal j (Move Baixo) m) m, imagens, tempo)
event (EventKey (SpecialKey KeyLeft) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo) = (PaginaJogar d, Jogo (deslocafinal j (Move Esquerda) m) m, imagens, tempo)
event (EventKey (SpecialKey KeyRight) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo) = (PaginaJogar d, Jogo (deslocafinal j (Move Direita) m) m, imagens, tempo)
event (EventKey (SpecialKey KeySpace) Down _ _) (PaginaJogar d, jogo, imagens, tempo) = (PaginaPausa Continuar_1 d, jogo, imagens, tempo)
event _ s = s


{-| A funcao 'reageTempo' registra a passagem do tempo e atualiza o mapa com a ajuda das auxiliares 'deslizaJogo' para deslizar o jogo e a 'animaJogo' para mover os 'Obstaculos' como 'Troncos' e 'Carros'

==codigo: 
@
reageTempo :: Float -> Mundo -> Mundo 
novoMundoReageTempo z (PaginaJogar, Jogo j m, imagens, t,e) = (PaginaJogar, (deslizaJogo ((round(t+z))*200) (animaJogo (Jogo j m) Parado)), imagens, (t+z))
@
-}

reageTempo :: Float -> Mundo -> Mundo 
reageTempo z (PaginaJogar Facil, Jogo j m, imagens, t) = (PaginaJogar Facil, (deslizaJogo ((round(t+z))*200) (animaJogo (Jogo j m) Parado)), imagens, (t+z))
reageTempo z (PaginaJogar Media, Jogo j m, imagens, t) = (PaginaJogar Media, (deslizaJogo ((round(t+z))*200) (animaJogo (Jogo j m) Parado)), imagens, (t+z))
reageTempo z (PaginaJogar Dificil, Jogo j m, imagens, t) = (PaginaJogar Dificil, (deslizaJogo ((round(t+z))*200) (animaJogo (Jogo j m) Parado)), imagens, (t+z))
reageTempo z (PaginaPrincipal c, jogo, imagens, t) = (PaginaPrincipal c, jogo, imagens, (t+z))
reageTempo _ z = z

{-| Variavel 'window', contem as definicoes do tamanho da tela, e neste caso vamos optar pelo Fullscreen que aproveita toda tela, para maior e melhor interaccao com o jogo

==codigo:
@
window :: Display
window = FullScreen
@
-}
window :: Display
window = FullScreen

{-| Variaver 'fr' contem o numero de frames por segundo em que o nosso programa vai funcionar 

==codigo: 
@
fr :: Int
fr = 1
@
-}

fr :: Int
fr = 1

{-| Variavel 'cor' contem a cor do background do nosso programa

==codigo:
@
cor :: Color
cor = black
@ 
-}

cor :: Color
cor = black


{-| Funcao 'main' e a funcao que contem todos os 'bmps' do jogo, e executa a funcao play, que vai carregar para o ecra todos os menus, e jogos, para que o user possa interagir com o programa

==codigo:
@

@
-}
main :: IO ()
main = do 
         galinha <- loadBMP "Chicken_JE2_BE2.bmp"
         rio <- loadBMP "water-surface-texture-1928713.bmp"
         relva <- loadBMP "textura-da-grama-verde-textura-do-relvado-96665200.bmp"
         estrada <- loadBMP "textura-da-estrada-com-linhas-10054832(1).bmp"
         banner <- loadBMP "Banner_Video_Cover.bmp"
         galinha4 <- loadBMP "arvore.bmp"
         rio2 <- loadBMP "rio.bmp"
         estrada2 <-loadBMP "estrada.bmp"
         relva2 <- loadBMP "relva.bmp"
         arvore2 <- loadBMP "arvore.bmp"
         tronco2 <- loadBMP "troncoinicio.bmp"
         carro2 <- loadBMP "mota.bmp"
         let imagens = [galinha28,{--scale 0.041 0.041 $ --}rio28, relva28, estrada28, banner, tronco28, arvore28, carro28, nenhum28,galinha2,galinha3,banner1,banner2,scale 0.12 0.12 $  relva2,Translate 0 25 $ scale 0.12 0.12 $ arvore2,scale 0.12 0.12 $ rio2, scale 0.12 0.12 $ tronco2,scale 0.12 0.12 $  estrada2,scale 0.12 0.12 $ carro2]
         let tempo = 0.0 
         let jogada = (Parado)        
         play window cor fr (estadoInicial imagens tempo) desenhaMundo event reageTempo

rio28 :: Picture 
rio28 = Color blue $ rectangleSolid lado lado  
relva28 :: Picture 
relva28 = Color green $ rectangleSolid lado lado  
estrada28 :: Picture 
estrada28 = Color black $ rectangleSolid lado lado 
tronco28 :: Picture
tronco28 = Color yellow $ rectangleSolid 30.0 50.0
arvore28 :: Picture
arvore28 = Color orange $ rectangleSolid 30.0 62.0
carro28 :: Picture
carro28 = Color white $ rectangleSolid 60.0 30.0
nenhum28 :: Picture
nenhum28 = Blank
galinha28 :: Picture
galinha28 = Color white $ thickCircle 5.0 10.0
galinha2 :: Picture
galinha2 = Color black $ thickCircle 5.0 10.0
galinha3 :: Picture
galinha3 = Color cyan $ thickCircle 5.0 10.0
banner1 :: Picture 
banner1 = Color blue $ rectangleSolid 300.0 300.0  
banner2 :: Picture 
banner2 = Color green $ rectangleSolid 300.0 300.0  

jogo1= Jogo (Jogador (5,8)) (Mapa 12 [(Estrada (1),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum])])