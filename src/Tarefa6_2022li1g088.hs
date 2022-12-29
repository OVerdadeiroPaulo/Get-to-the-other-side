{-module      : Tarefa3_2022li1g088
Description : Funcao para fazer o mapa deslizar continuamente 
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}


module Main where

import LI12223
import Tarefa1_2022li1g088
import Tarefa2_2022li1g088
import Tarefa3_2022li1g088
import Tarefa4_2022li1g088
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Mundo = (Paginas, Jogo, Imagens, Float)

type Imagens = [Picture]

{-| Menu que aparece quando entras no jogo -}
data MenuPrincipal = Jogar -- ^ Opcao para ir directamente ao jogo no nivel Facil ja predifinido 
                   | Instrucoes_1 -- ^ Opcao para ver as instrucoes e objectivos do jogo 
                   | Sair_1 -- ^ Opcao para sair do jogo 
                  deriving (Eq)

{-| Menu onde escolhes o nivel de dificuldade no jogo -}
data Dificuldade = Facil -- ^ Opcao para jogar em um nivel de dificuldade facil
                 | Media -- ^ Opcao para jogar em um nivel de dificuldade medio 
                 | Dificil -- ^ Opcao para jogar no nivel de dificuldade mais dificil 
                 | Menu1 -- ^ opcao para voltar ao menu principal
                deriving (Eq) 

{-| Menu de Pausa, onde o jogo fica parado e o jogador pode escolher continuar o jogo ou voltar ao menu principal -}
data Pausa = Continuar_1 -- ^ Opcao para continuar o jogo pendente  
           | Menu_2 -- ^ Opcao para voltar ao menu principal

{-| Menu que aparece quando perdes o jogo -}
data MenuMorte = Reniciar -- ^ Opcao para reniciar o jogo na mesma dificuldade
               | Menu_3 -- ^ Opcao para Voltar ao menu principal

{-| Menu Principal quando o jogador tem um jogo pendente -}
data MenuPausa = Continuar_2 -- ^ Opcao para continuar o jogo pendente 
               | NovoJogo -- ^ Opcao para comecar um novo jogo e esquecer o pendente 
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

estadoInicial :: Imagens -> Float -> Jogada -> Mundo 
estadoInicial imagens tempo jogada = (PaginaJogar, jogo1, imagens,tempo, jogada)




desenhaMundo :: Mundo -> Picture
--PaginaPrincipal 
desenhaMundo (PaginaPrincipal Jogar, jogo, imagens, tempo) = fundoAnimado1 {--Pictures [Scale 1.0 1.0 (imagens !! 4)] --}
            where fundoAnimado1 = imagefundo (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
desenhaMundo (PaginaPrincipal Dificuldades_1, jogo, imagens) = fundoAnimado2    {-Picture [Scale 1.0 1.0 (imagens !! 4)]-} 
            where fundoAnimado2 = imagefundo (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
desenhaMundo (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo) = fundoAnimado3   {-Pictures [Scale 1.0 1.0 (imagens !! 4)]-} 
            where fundoAnimado3 = imagefundo2 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
desenhaMundo (PaginaPrincipal Sair_1, jogo, imagens, tempo) = fundoAnimado4  {--Pictures [Scale 1.0 1.0 (imagens !! 4)]--}
            where fundoAnimado4 = imagefundo (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo)) (getImagens (PaginaPrincipal Jogar, jogo, imagens, tempo))
--PaginaPerdeuJogo
desenhaMundo (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens) = Picture [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
--PaginaPausa
desenhaMundo (PaginaPausa Continuar_1 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaPausa Menu_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
--PaginaDificuldade
desenhaMundo (PaginaDificuldade Facil b d, jogo, imagens, tempo) = Picture [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaDificuldade Media b d, jogo, imagens, tempo) = Picture [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaDificuldade Dificil b d, jogo, imagens, tempo) = Picture [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaDificuldade Menu1 b d, jogo, imagens, tempo) = Picture [Scale 1.0 1.0 (imagens !! 4)] 
--paginaInstrucoes 
desenhaMundo (PaginaInstrucoes b d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
--PaginaMenuPaus
desenhaMundo (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa Dificuldades_2 d, jogo, imagens) = Picture [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
--PaginaJogar 
desenhaMundo (PaginaJogar d, jogo, imagens, tempo) | d == Facil = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world28 
                                                   | d == Media = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world29
                                                   | d == Dificil = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world30
 where 
     world28 = desenhaTerrenos ++ desenhaObstaculos ++ [tempoDeJogo] ++[scoore] ++ [desenhajogador] -- ^ Mundo da dificuldade Facil 
     desenhaTerrenos = criarTerreno p o (getLargura(getMapa (PaginaJogar, jogo, imagens, tempo))) (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens
     desenhaObstaculos = criarObstaculos p o (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens 
     desenhajogador = criarJogador (getJogador (PaginaJogar, jogo, imagens, tempo)) (getTempo (PaginaJogar, jogo, imagens, tempo)) imagens
     tempoDeJogo = mostrarTempo (getTempo(PaginaJogar, jogo, imagens, tempo))
     scoore = mostrarScoore (getTempo(PaginaJogar, jogo, imagens, tempo)) 
    
     world29 = desenhaTerrenos ++ desenhaObstaculos ++ [tempoDeJogo] ++ [scoore] ++ [desenhajogador] -- ^ Mundo da dificuldade Media
     desenhaTerrenos = criarTerreno p o (getLargura(getMapa (PaginaJogar, jogo, imagens, tempo))) (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens
     desenhaObstaculos = criarObstaculos p o (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens 
     desenhajogador = criarJogador (getJogador (PaginaJogar, jogo, imagens, tempo)) (getTempo (PaginaJogar, jogo, imagens, tempo)) imagens
     tempoDeJogo = mostrarTempo (getTempo(PaginaJogar, jogo, imagens, tempo))
     scoore = mostrarScoore (getTempo(PaginaJogar, jogo, imagens, tempo))
    
     world30 = desenhaTerrenos ++ desenhaObstaculos ++ [tempoDeJogo] ++ [scoore]++ [desenhajogador] -- ^ Mundo da dificuldade Dificil
     desenhaTerrenos = criarTerreno p o (getLargura(getMapa (PaginaJogar, jogo, imagens, tempo))) (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens
     desenhaObstaculos = criarObstaculos p o (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens 
     desenhajogador = criarJogador (getJogador (PaginaJogar, jogo, imagens, tempo)) (getTempo (PaginaJogar, jogo, imagens, tempo)) imagens
     tempoDeJogo = mostrarTempo (getTempo(PaginaJogar, jogo, imagens, tempo))
     scoore = mostrarScoore (getTempo(PaginaJogar, jogo, imagens, tempo))

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

{-| Funcao 'criarTerreno1' esta funcao cria o Mapa usando o 'desenhalinha1' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Terrenos' desejados para a dificuldade 'Facil'

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

{-| Funcao 'criarTerreno2' esta funcao cria o Mapa usando o 'desenhalinha2' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Terrenos' desejados para a dificuldade 'Media'

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

{-| Funcao 'criarTerreno3' esta funcao cria o Mapa usando o 'desenhalinha2' como auxiliar, ela adicina as linhas criadas umas abaixo das outras, completando a tela com os 'Terrenos' desejados para a dificuldade 'Media'

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



jogo1= Jogo (Jogador (5,8)) (Mapa 12 [(Estrada (1),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum])])