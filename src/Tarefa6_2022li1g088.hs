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

type Mundo = (Paginas, Jogo, Imagens, Float, Jogada, Imagens, Imagens, Imagens)

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

estadoInicial :: Imagens -> Float -> Jogada -> Imagens -> Imagens -> Imagens -> Mundo 
estadoInicial imagens tempo direccao imagens2 imagens3 imagens4 = (PaginaPrincipal Dificuldades_1, jogo1, imagens, tempo, direccao, imagens2, imagens3, imagens4)



{-| A funcao 'desenhaMundo' devolve a picture do 'Mundo' no ecra e nos permite ver o resultado das nossas interacoes com o programa

==codigo:
@

@
-}
desenhaMundo :: Mundo -> Picture
--PaginaPrincipal 
desenhaMundo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++ [pagPrincipal])
            where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                  pagPrincipal = pgPrincipal imagens4
desenhaMundo (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++ [pagPrincipalDificuldade]) 
            where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                  pagPrincipalDificuldade = pgPrincipal2 imagens4
desenhaMundo (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++ [pagPrincipal3]) 
            where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                  pagPrincipal3 = pgPrincipal3 imagens4
desenhaMundo (PaginaPrincipal Sair_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagPrincipal4])
            where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                  pagPrincipal4 = pgPrincipal4 imagens4
--PaginaPerdeuJogo
desenhaMundo (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagPerdeu1])
              where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                    pagPerdeu1 = pgPerdeu1 imagens4
desenhaMundo (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagPerdeu2])
              where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                    pagPerdeu2 = pgPerdeu2 imagens4
desenhaMundo (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagPerdeu2])
              where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                    pagPerdeu2 = pgPerdeu3 imagens4
--PaginaPausa
desenhaMundo (PaginaPausa Continuar_1 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagPausa1])
              where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                    pagPausa1 = pgPausa1 imagens4
desenhaMundo (PaginaPausa Menu_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagPausa2])
              where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                    pagPausa2 = pgPausa2 imagens4
--PaginaDificuldade
desenhaMundo (PaginaDificuldade Facil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagDificuldade1])
              where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                    pagDificuldade1 = pgDificuldade1 imagens4
desenhaMundo (PaginaDificuldade Media b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagDificuldade2])
              where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                    pagDificuldade2 = pgDificuldade2 imagens4
desenhaMundo (PaginaDificuldade Dificil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagDificuldade3]) 
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagDificuldade3 = pgDificuldade3 imagens4
desenhaMundo (PaginaDificuldade Menu1 b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagDificuldade4]) 
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagDificuldade4 = pgDificuldade4 imagens4
--paginaInstrucoes 
desenhaMundo (PaginaInstrucoes b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagInstrucoes])
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagInstrucoes = pgInstrucoes imagens4
--PaginaMenuPausa
desenhaMundo (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagNova1])
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagNova1 = pgNova1 imagens4
desenhaMundo (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagNova2])
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagNova2 = pgNova2 imagens4
desenhaMundo (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagNova3])
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagNova3 = pgNova3 imagens4
desenhaMundo (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagNova4])
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagNova4 = pgNova4 imagens4
desenhaMundo (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = Pictures ([fundoAnimado1] ++[pagNova5])
             where fundoAnimado1 = imagefundo1 (getTempo (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens4
                   pagNova5 = pgNova5 imagens4
--PaginaJogar 
desenhaMundo (PaginaJogar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) | d == Facil = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world28 
                                                                                           | d == Media = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world29
                                                                                           | d == Dificil = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world30
 where 
     world28 = desenhaTerrenos1 ++ [desenhajogador1] ++ desenhaObstaculos1 ++ [mostrarPlacar] ++ [tempoDeJogo1] ++[scoore1] -- ^ Mundo da dificuldade Facil 
     desenhaTerrenos1 = criarTerreno1 p o (getLargura(getMapa (PaginaJogar Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) (getTerreno(getMapa (PaginaJogar Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) imagens
     desenhaObstaculos1 = criarObstaculos1 p o (getTerreno(getMapa (PaginaJogar Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) imagens 
     desenhajogador1 = criarJogador1 (getJogador (PaginaJogar Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) (getTempo (PaginaJogar Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens (getDireccao (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))
     tempoDeJogo1 = mostrarTempo (getTempo(PaginaJogar Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))
     scoore1 = mostrarScoore (getTempo(PaginaJogar Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) 
    
     world29 = desenhaTerrenos2 ++ [desenhajogador2] ++ desenhaObstaculos2 ++ [mostrarPlacar] ++[tempoDeJogo2] ++ [scoore2] -- ^ Mundo da dificuldade Media
     desenhaTerrenos2 = criarTerreno2 p o (getLargura(getMapa (PaginaJogar Media, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) (getTerreno(getMapa (PaginaJogar Media, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) imagens
     desenhaObstaculos2 = criarObstaculos2 p o (getTerreno(getMapa (PaginaJogar Media, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) imagens 
     desenhajogador2 = criarJogador2 (getJogador (PaginaJogar Media, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) (getTempo (PaginaJogar Media, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens2 (getDireccao (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))
     tempoDeJogo2 = mostrarTempo (getTempo(PaginaJogar Media, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))
     scoore2 = mostrarScoore (getTempo(PaginaJogar Media, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))
    
     world30 = desenhaTerrenos3 ++ [desenhajogador3] ++ desenhaObstaculos3 ++ [placar] ++ [tempoDeJogo3] ++ [scoore3] -- ^ Mundo da dificuldade Dificil
     desenhaTerrenos3 = criarTerreno3 p o (getLargura(getMapa (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) (getTerreno(getMapa (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) imagens
     desenhaObstaculos3 = criarObstaculos3 p o (getTerreno(getMapa (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))) imagens 
     desenhajogador3 = criarJogador3 (getJogador (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) (getTempo (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) imagens3 (getDireccao (PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)) 
     tempoDeJogo3 = mostrarTempo (getTempo(PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))
     scoore3 = mostrarScoore (getTempo(PaginaJogar Dificil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4))
     placar = mostrarPlacar 


pgPrincipal :: Imagens -> Picture 
pgPrincipal imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 50)] ++ [Translate 0.0 0.0 $ (imagens4 !! 43)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 47)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 57)])

pgPrincipal2 :: Imagens -> Picture 
pgPrincipal2 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 49)] ++ [Translate 0.0 0.0 $ (imagens4 !! 44)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 47)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 57)])

pgPrincipal3 :: Imagens -> Picture 
pgPrincipal3 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 49)] ++ [Translate 0.0 0.0 $ (imagens4 !! 43)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 48)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 57)])

pgPrincipal4 :: Imagens -> Picture 
pgPrincipal4 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 49)] ++ [Translate 0.0 0.0 $ (imagens4 !! 43)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 47)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 58)])

pgPerdeu1 :: Imagens -> Picture 
pgPerdeu1 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 56)] ++ [Translate 0.0 0.0 $ (imagens4 !! 43)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 53)])

pgPerdeu2 :: Imagens -> Picture 
pgPerdeu2 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 55)] ++ [Translate 0.0 0.0 $ (imagens4 !! 44)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 53)])

pgPerdeu3 :: Imagens -> Picture 
pgPerdeu3 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 55)] ++ [Translate 0.0 0.0 $ (imagens4 !! 43)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 54)])

pgPausa1 :: Imagens -> Picture 
pgPausa1 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 40)] ++ [Translate 0.0 0.0 $ (imagens4 !! 53)])

pgPausa2 :: Imagens -> Picture 
pgPausa2 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 39)] ++ [Translate 0.0 0.0 $ (imagens4 !! 54)])

pgDificuldade1 :: Imagens -> Picture 
pgDificuldade1 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 46)] ++ [Translate 0.0 0.0 $ (imagens4 !! 51)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 41)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 57)])

pgDificuldade2 :: Imagens -> Picture 
pgDificuldade2 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 45)] ++ [Translate 0.0 0.0 $ (imagens4 !! 52)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 41)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 57)])

pgDificuldade3 :: Imagens -> Picture 
pgDificuldade3 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 45)] ++ [Translate 0.0 0.0 $ (imagens4 !! 51)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 42)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 57)])

pgDificuldade4 :: Imagens -> Picture 
pgDificuldade4 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 45)] ++ [Translate 0.0 0.0 $ (imagens4 !! 51)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 41)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 58)])

pgNova1 :: Imagens -> Picture 
pgNova1 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 40)] ++ [Translate 0.0 0.0 $ (imagens4 !! 55)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 43)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 47)] ++ [Translate 0.0 (-240.0) $ (imagens4 !! 57)])

pgNova2 :: Imagens -> Picture 
pgNova2 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 39)] ++ [Translate 0.0 0.0 $ (imagens4 !! 56)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 43)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 47)] ++ [Translate 0.0 (-240.0) $ (imagens4 !! 57)])

pgNova3 :: Imagens -> Picture 
pgNova3 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 39)] ++ [Translate 0.0 0.0 $ (imagens4 !! 55)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 44)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 47)] ++ [Translate 0.0 (-240.0) $ (imagens4 !! 57)])

pgNova4 :: Imagens -> Picture 
pgNova4 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 39)] ++ [Translate 0.0 0.0 $ (imagens4 !! 55)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 43)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 48)] ++ [Translate 0.0 (-240.0) $ (imagens4 !! 57)])

pgNova5 :: Imagens -> Picture 
pgNova5 imagens4 = Pictures ([Translate 0.0 80.0 $ (imagens4 !! 39)] ++ [Translate 0.0 0.0 $ (imagens4 !! 55)] ++ [Translate 0.0 (-80.0) $ (imagens4 !! 43)] ++ [Translate 0.0 (-160.0) $ (imagens4 !! 47)] ++ [Translate 0.0 (-240.0) $ (imagens4 !! 58)])

pgInstrucoes :: Imagens -> Picture 
pgInstrucoes imagens4 = Pictures ([Translate 0.0 (-80.0) $ (imagens4 !! 59)]) 

{-| A funcao 'mostraTempo' mostra o tempo a ser registrado no jogo a passar

== Codigo:
@
mostrarTempo :: Float -> Picture
mostrarTempo t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)
@
-}

mostrarTempo :: Float -> Picture
mostrarTempo t =  Translate 620 (-80) $ scale 0.2 0.2 $ color red $ Text (show $ round t)

{-| A funcao 'mostraTempo' mostra o tempo a ser registrado no jogo a passar

== Codigo:
@
mostrarScoore :: Float -> Picture
mostrarScoore t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)
@
-}

mostrarScoore :: Float -> Picture
mostrarScoore t =  Translate 620 (-160) $ scale 0.2 0.2 $ color white $ Text (show $ round (t*124))

{-| A funcao 'mostraPlacar' mostra o tempo no jogo

== Codigo:
@
mostrarPlacar :: Float -> Picture
mostrarPlacar t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)
@
-}

mostrarPlacar :: Picture
mostrarPlacar =  Translate 650 (-110) $ color black $ rectangleSolid 100.0 150.0

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
imagefundo1 t imagens4 |(mod (round (t*22222)) 400) < 10 = (imagens4 !! 0)
                       | (mod (round (t*22222)) 400) < 20 = (imagens4 !! 1)
                       | (mod (round (t*22222)) 400) < 30 = (imagens4 !! 2)
                       | (mod (round (t*22222)) 400) < 40 = (imagens4 !! 3)
                       | (mod (round (t*22222)) 400) < 50 = (imagens4 !! 4)
                       | (mod (round (t*22222)) 400) < 60 = (imagens4 !! 5)
                       | (mod (round (t*22222)) 400) < 70 = (imagens4 !! 6)
                       | (mod (round (t*22222)) 400) < 80 = (imagens4 !! 7)
                       | (mod (round (t*22222)) 400) < 90 = (imagens4 !! 8)
                       | (mod (round (t*22222)) 400) < 100 = (imagens4 !! 9)
                       | (mod (round (t*22222)) 400) < 110 = (imagens4 !! 10)
                       | (mod (round (t*22222)) 400) < 120 = (imagens4 !! 11)
                       | (mod (round (t*22222)) 400) < 130 = (imagens4 !! 12)
                       | (mod (round (t*22222)) 400) < 140 = (imagens4 !! 13)
                       | (mod (round (t*22222)) 400) < 150 = (imagens4 !! 14)
                       | (mod (round (t*22222)) 400) < 160 = (imagens4 !! 15)
                       | (mod (round (t*22222)) 400) < 170 = (imagens4 !! 16)
                       | (mod (round (t*22222)) 400) < 180 = (imagens4 !! 17)
                       | (mod (round (t*22222)) 400) < 190 = (imagens4 !! 18)
                       | (mod (round (t*22222)) 400) < 200 = (imagens4 !! 19)
                       | (mod (round (t*22222)) 400) < 210 = (imagens4 !! 20)
                       | (mod (round (t*22222)) 400) < 220 = (imagens4 !! 21)
                       | (mod (round (t*22222)) 400) < 230 = (imagens4 !! 22)
                       | (mod (round (t*22222)) 400) < 240 = (imagens4 !! 23)
                       | (mod (round (t*22222)) 400) < 250 = (imagens4 !! 24)
                       | (mod (round (t*22222)) 400) < 260 = (imagens4 !! 25)
                       | (mod (round (t*22222)) 400) < 270 = (imagens4 !! 26)
                       | (mod (round (t*22222)) 400) < 280 = (imagens4 !! 27)
                       | (mod (round (t*22222)) 400) < 290 = (imagens4 !! 28)
                       | (mod (round (t*22222)) 400) < 300 = (imagens4 !! 29)
                       | (mod (round (t*22222)) 400) < 310 = (imagens4 !! 30)
                       | (mod (round (t*22222)) 400) < 320 = (imagens4 !! 31)
                       | (mod (round (t*22222)) 400) < 330 = (imagens4 !! 32)
                       | (mod (round (t*22222)) 400) < 340 = (imagens4 !! 33)
                       | (mod (round (t*22222)) 400) < 350 = (imagens4 !! 34)
                       | (mod (round (t*22222)) 400) < 360 = (imagens4 !! 35)
                       | (mod (round (t*22222)) 400) < 370 = (imagens4 !! 36)
                       | (mod (round (t*22222)) 400) < 380 = (imagens4 !! 37)
                       | otherwise = (imagens4 !! 38)
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
imagefundo2 t imagens | (mod (round (t)) 3) < 0 = (imagens !! 34)
                      | (mod (round (t)) 3) < 1 = (imagens !! 28)
                      | otherwise = (imagens !! 34)
-- imagefundo2 :: Float -> Imagens -> Picture 
-- imagefundo2 t imagens | (mod (round (t*1000)) 3000) < 1000 = (imagens !! 34)
--                       | (mod (round (t*1000)) 3000) > 2000 = (imagens !! 28)
--                       | otherwise = (imagens !! 34)

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
imagefundo3 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 27)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 28)
                      | otherwise = (imagens !! 29)

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
imagefundo4 t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 27)
                      | (mod (round (t*1000)) 300) > 200 = (imagens !! 28)
                      | otherwise = (imagens !! 29)                      

{-| A funcao 'getImage' busca a imagem no 'Mundo'

== Codigo: 
@
getImagens :: Mundo -> Imagens
getImagens (_, _, i, _) = i
@
-}

getImagens :: Mundo -> Imagens
getImagens (_, _, i, _, _, _, _, _) = i


{-| A funcao auxiliar 'getTempo' busca o tempo no 'Mundo'
@
getTempo :: Mundo -> Float 
getTempo (_, _, _, tempo) = tempo
@
-}

getTempo :: Mundo -> Float 
getTempo (_, _, _, tempo, _, _,_, _) = tempo 

{-| A funcao 'getMapa' extrai o Mapa do 'Jogo' que esta dentro do 'Mundo' 

== Codigo:
@
getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _, _) = m
@ 
-}

getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _, _, _, _, _, _) = m 

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
getJogador (_, Jogo j m, _, _, _, _, _, _) = j

{-| A Funcao 'getDireccao' extrai o 'Jogador' do 'Mundo'

== Codigo:
@
getDireccao :: Mundo -> Jogador
getDireccao (_, _, _, _, _) = j
@
-}

getDireccao :: Mundo -> Jogada
getDireccao (_, _, _, _, dr, _, _, _) = dr

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
 | inicionovo terreno == "Rel" = (imagens !! 4)
 | inicionovo terreno == "Rio" = (imagens !! 3)
 | inicionovo terreno == "Est" = (imagens !! 5)
@
-}

renderTer1 :: Terreno -> Imagens -> Picture 
renderTer1 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 4)
 | inicionovo terreno == "Rio" = (imagens !! 3)
 | inicionovo terreno == "Est" = (imagens !! 5)
 
{-| Funcao 'renderTer2' e a funcao auxiliar da funcao 'desenhaTer2' funcao junta cada 'Terreno' a uma 'Imagem' ja definida para a dificuldade 'Media' 

== Codigo: 
@
renderTer2 :: Terreno -> Imagens -> Picture 
renderTer2 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 11)
 | inicionovo terreno == "Rio" = (imagens !! 9)
 | inicionovo terreno == "Est" = (imagens !! 10)
@
-}

renderTer2 :: Terreno -> Imagens -> Picture 
renderTer2 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 11)
 | inicionovo terreno == "Rio" = (imagens !! 9)
 | inicionovo terreno == "Est" = (imagens !! 10)

{-| Funcao 'renderTer3' e a funcao auxiliar da funcao 'desenhaTer3' funcao junta cada 'Terreno' a uma 'Imagem' ja definida para a dificuldade 'Dificil' 

== Codigo: 
@
renderTer3 :: Terreno -> Imagens -> Picture 
renderTer3 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 19)
 | inicionovo terreno == "Rio" = (imagens !! 18)
 | inicionovo terreno == "Est" = (imagens !! 20)
@
-}

renderTer3 :: Terreno -> Imagens -> Picture 
renderTer3 terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 19)
 | inicionovo terreno == "Rio" = (imagens !! 18)
 | inicionovo terreno == "Est" = (imagens !! 20)

{-| Funcao 'renderObs1' e a funcao auxiliar da funcao 'desenhaObs1', esta funcao junta cada 'Obstaculo' a uma 'Imagem' ja definida para a dificuldade 'Facil'

== Codigo:
@
renderObs1 :: Obstaculo -> Imagens -> Picture 
renderObs1 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 30)
 | obstaculo == Tronco = (imagens !! 25)
 | obstaculo == Arvore = (imagens !! 23)
 | obstaculo == Carro = (imagens !! 22)
@
-}

renderObs1 :: Obstaculo -> Imagens -> Picture 
renderObs1 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 30)
 | obstaculo == Tronco = (imagens !! 25)
 | obstaculo == Arvore = (imagens !! 23)
 | obstaculo == Carro = (imagens !! 22)

{-| Funcao 'renderObs2' e a funcao auxiliar da funcao 'desenhaObs2', esta funcao junta cada 'Obstaculo' a uma 'Imagem' ja definida para a dificuldade 'Media'

== Codigo:
@
renderObs2 :: Obstaculo -> Imagens -> Picture 
renderObs2 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 30)
 | obstaculo == Tronco = (imagens !! 13)
 | obstaculo == Arvore = (imagens !! 12)
 | obstaculo == Carro = (imagens !! 14)
@
-}

renderObs2 :: Obstaculo -> Imagens -> Picture 
renderObs2 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 30)
 | obstaculo == Tronco = (imagens !! 13)
 | obstaculo == Arvore = (imagens !! 12)
 | obstaculo == Carro = (imagens !! 14)

{-| Funcao 'renderObs3' e a funcao auxiliar da funcao 'desenhaObs3', esta funcao junta cada 'Obstaculo' a uma 'Imagem' ja definida para a dificuldade 'Media'

== Codigo:
@
renderObs3 :: Obstaculo -> Imagens -> Picture 
renderObs3 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 30)
 | obstaculo == Tronco = (imagens !! 26)
 | obstaculo == Arvore = (imagens !! 24)
 | obstaculo == Carro = (imagens !! 21)
@
-}

renderObs3 :: Obstaculo -> Imagens -> Picture 
renderObs3 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 30)
 | obstaculo == Tronco = (imagens !! 26)
 | obstaculo == Arvore = (imagens !! 24)
 | obstaculo == Carro = (imagens !! 21)

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

                                                                          
criarJogador1 :: Jogador -> Float-> Imagens -> Jogada -> Picture
criarJogador1 (Jogador (x,y)) t imagens (Move Cima)  |(mod (round (t*22222)) 40) < 1 = Translate (saltaX x) (saltaY y) (imagens !! 34)
                                                     | (mod (round (t*22222)) 40) < 2 = Translate (saltaX x) (saltaY y) (imagens !! 35)
                                                     | (mod (round (t*22222)) 40) < 3 = Translate (saltaX x) (saltaY y) (imagens !! 36)
                                                     | (mod (round (t*22222)) 40) < 4 = Translate (saltaX x) (saltaY y) (imagens !! 37)
                                                     | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens !! 38)
                                                     | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens !! 39)
                                                     | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens !! 40)
                                                     | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens !! 41)
                                                     | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens !! 42)
                                                     | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 43)
                                                     | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 44)
                                                     | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 45)
                                                     | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 46)
                                                     | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 47)
                                                     | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 48)
                                                     | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 49)
                                                     | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 50)
                                                     | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 51)
                                                     | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 52)
                                                     | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 53)
                                                     | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 54)
                                                     | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 55)
                                                     | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 56)
                                                     | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 57)
                                                     | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 58)
                                                     | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 59)
                                                     | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 60)
                                                     | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 61)
                                                     | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 62)
                                                     | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 63)
                                                     | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens !! 64)
                                                     | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens !! 65)
                                                     | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens !! 66)
                                                     | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens !! 67)
                                                     | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens !! 68)
                                                     | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens !! 69)
                                                     | (mod (round (t*22222)) 40) < 37 = Translate (saltaX x) (saltaY y) (imagens !! 70)
                                                     | (mod (round (t*22222)) 40) < 38 = Translate (saltaX x) (saltaY y) (imagens !! 71)
                                                     | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 72)
criarJogador1 (Jogador (x,y)) t imagens (Move Esquerda) | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 81)
                                                        | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 82)
                                                        | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 83)
                                                        | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 84)
                                                        | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 85)
                                                        | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 86)
                                                        | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 87)
                                                        | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 88)
                                                        | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 89)
                                                        | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 90)
                                                        | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 91)
                                                        | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 92)
                                                        | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 93)
                                                        | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 94)
                                                        | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 95)
                                                        | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 96)
                                                        | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 97)
                                                        | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 98)
                                                        | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 99)
                                                        | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 100)
                                                        | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 101)
                                                        | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 101)  
criarJogador1 (Jogador (x,y)) t imagens (Move Direita)  | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens !! 118)
                                                        | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 119)
                                                        | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 120)
                                                        | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 121)
                                                        | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 122)
                                                        | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 123)
                                                        | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 124)
                                                        | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 125)
                                                        | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 126)
                                                        | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 127)
                                                        | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 128)
                                                        | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 129)
                                                        | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 130)
                                                        | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 131)
                                                        | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 132)
                                                        | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 133)
                                                        | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 134)
                                                        | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 135)
                                                        | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 136)
                                                        | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 137)
                                                        | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 140)
                                                        | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 141)
                                                        | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 141)

 
{-| Funcao 'criarJogador2' pega no jogador, num float e numa imagem e devolve a picture do jogador, e com a ajuda das auxiliares 'saltaX' e 'saltaY' ela tranlada o jogador sempre para o centro do novo bloco de 'Terreno' e por causa das guardas e do '(mod (round (t*1000)) 300) < 100' conseguimos alternar entre as imagens do jogador criando um jogador em movimento perpetuo, na dificuldade 'Media'.

==codigo:
@
criarJogador2 :: Jogador -> Float-> Imagens -> Picture
criarJogador2 (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                       | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                       | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)  
@ 
-}
                                                                     

criarJogador2 :: Jogador -> Float-> Imagens -> Jogada -> Picture
criarJogador2 (Jogador (x,y)) t imagens2 (Move Cima)  |(mod (round (t*22222)) 40) < 1 = Translate (saltaX x) (saltaY y) (imagens2 !! 0)
                                                      | (mod (round (t*22222)) 40) < 2 = Translate (saltaX x) (saltaY y) (imagens2 !! 2)
                                                      | (mod (round (t*22222)) 40) < 3 = Translate (saltaX x) (saltaY y) (imagens2 !! 3)
                                                      | (mod (round (t*22222)) 40) < 4 = Translate (saltaX x) (saltaY y) (imagens2 !! 4)
                                                      | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens2 !! 5)
                                                      | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens2 !! 6)
                                                      | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens2 !! 7)
                                                      | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens2 !! 8)
                                                      | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens2 !! 9)
                                                      | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens2 !! 10)
                                                      | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens2 !! 11)
                                                      | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens2 !! 12)
                                                      | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens2 !! 13)
                                                      | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens2 !! 14)
                                                      | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens2 !! 15)
                                                      | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens2 !! 16)
                                                      | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens2 !! 17)
                                                      | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens2 !! 18)
                                                      | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens2 !! 19)
                                                      | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens2 !! 20)
                                                      | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens2 !! 21)
                                                      | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens2 !! 22)
                                                      | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens2 !! 23)
                                                      | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens2 !! 24)
                                                      | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens2 !! 25)
                                                      | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens2 !! 26)
                                                      | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens2 !! 27)
                                                      | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens2 !! 28)
                                                      | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens2 !! 29)
                                                      | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens2 !! 30)
                                                      | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens2 !! 31)
                                                      | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens2 !! 32)
                                                      | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens2 !! 33)
                                                      | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens2 !! 34)
                                                      | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens2 !! 35)
                                                      | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens2 !! 36)
                                                      | (mod (round (t*22222)) 40) < 37 = Translate (saltaX x) (saltaY y) (imagens2 !! 37)
                                                      | otherwise = Translate (saltaX x) (saltaY y) (imagens2 !! 2)
criarJogador2 (Jogador (x,y)) t imagens2 (Move Esquerda) | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens2 !! 39)
                                                         | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens2 !! 40)
                                                         | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens2 !! 41)
                                                         | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens2 !! 42)
                                                         | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens2 !! 43)
                                                         | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens2 !! 44)
                                                         | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens2 !! 45)
                                                         | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens2 !! 46)
                                                         | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens2 !! 47)
                                                         | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens2 !! 48)
                                                         | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens2 !! 49)
                                                         | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens2 !! 50)
                                                         | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens2 !! 51)
                                                         | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens2 !! 52)
                                                         | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens2 !! 53)
                                                         | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens2 !! 54)
                                                         | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens2 !! 55)
                                                         | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens2 !! 56)
                                                         | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens2 !! 57)
                                                         | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens2 !! 58)
                                                         | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens2 !! 59)
                                                         | otherwise = Translate (saltaX x) (saltaY y) (imagens2 !! 39)  
criarJogador2 (Jogador (x,y)) t imagens2 (Move Direita)  | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens2 !! 60)
                                                         | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens2 !! 61)
                                                         | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens2 !! 62)
                                                         | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens2 !! 63)
                                                         | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens2 !! 64)
                                                         | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens2 !! 65)
                                                         | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens2 !! 66)
                                                         | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens2 !! 67)
                                                         | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens2 !! 68)
                                                         | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens2 !! 69)
                                                         | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens2 !! 70)
                                                         | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens2 !! 71)
                                                         | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens2 !! 72)
                                                         | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens2 !! 73)
                                                         | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens2 !! 74)
                                                         | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens2 !! 75)
                                                         | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens2 !! 76)
                                                         | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens2 !! 77)
                                                         | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens2 !! 78)
                                                         | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens2 !! 79)
                                                         | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens2 !! 80)
                                                         | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens2 !! 81)
                                                         | otherwise = Translate (saltaX x) (saltaY y) (imagens2 !! 60)
 
 
{-| Funcao 'criarJogador3' pega no jogador, num float e numa imagem e devolve a picture do jogador, e com a ajuda das auxiliares 'saltaX' e 'saltaY' ela tranlada o jogador sempre para o centro do novo bloco de 'Terreno' e por causa das guardas e do '(mod (round (t*1000)) 300) < 100' conseguimos alternar entre as imagens do jogador criando um jogador em movimento perpetuo, na dificuldade 'Dificil'.

==codigo:
@
criarJogador3 :: Jogador -> Float-> Imagens -> Picture
criarJogador3 (Jogador (x,y)) t imagens | (mod (round (t*1000)) 300) < 100 = Translate (saltaX x) (saltaY y) (imagens !! 0)
                                       | (mod (round (t*1000)) 300) > 200 = Translate (saltaX x) (saltaY y) (imagens !! 10)
                                       | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 9)
@ 
-}
                                                                     
criarJogador3 (Jogador (x,y)) t imagens3 (Move Cima)  |(mod (round (t*22222)) 40) < 1 = Translate (saltaX x) (saltaY y) (imagens3 !! 0)
                                                      | (mod (round (t*22222)) 40) < 2 = Translate (saltaX x) (saltaY y) (imagens3 !! 2)
                                                      | (mod (round (t*22222)) 40) < 3 = Translate (saltaX x) (saltaY y) (imagens3 !! 3)
                                                      | (mod (round (t*22222)) 40) < 4 = Translate (saltaX x) (saltaY y) (imagens3 !! 4)
                                                      | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens3 !! 5)
                                                      | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens3 !! 6)
                                                      | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens3 !! 7)
                                                      | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens3 !! 8)
                                                      | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens3 !! 9)
                                                      | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens3 !! 10)
                                                      | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens3 !! 11)
                                                      | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens3 !! 12)
                                                      | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens3 !! 13)
                                                      | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens3 !! 14)
                                                      | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens3 !! 15)
                                                      | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens3 !! 16)
                                                      | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens3 !! 17)
                                                      | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens3 !! 18)
                                                      | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens3 !! 19)
                                                      | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens3 !! 20)
                                                      | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens3 !! 21)
                                                      | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens3 !! 22)
                                                      | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens3 !! 23)
                                                      | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens3 !! 24)
                                                      | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens3 !! 25)
                                                      | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens3 !! 26)
                                                      | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens3 !! 27)
                                                      | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens3 !! 28)
                                                      | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens3 !! 29)
                                                      | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens3 !! 30)
                                                      | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens3 !! 31)
                                                      | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens3 !! 32)
                                                      | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens3 !! 33)
                                                      | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens3 !! 34)
                                                      | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens3 !! 35)
                                                      | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens3 !! 36)
                                                      | (mod (round (t*22222)) 40) < 37 = Translate (saltaX x) (saltaY y) (imagens3 !! 37)
                                                      | otherwise = Translate (saltaX x) (saltaY y) (imagens3 !! 2)
criarJogador3 (Jogador (x,y)) t imagens3 (Move Esquerda) | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens3 !! 39)
                                                         | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens3 !! 40)
                                                         | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens3 !! 41)
                                                         | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens3 !! 42)
                                                         | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens3 !! 43)
                                                         | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens3 !! 44)
                                                         | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens3 !! 45)
                                                         | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens3 !! 46)
                                                         | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens3 !! 47)
                                                         | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens3 !! 48)
                                                         | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens3 !! 49)
                                                         | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens3 !! 50)
                                                         | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens3 !! 51)
                                                         | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens3 !! 52)
                                                         | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens3 !! 53)
                                                         | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens3 !! 54)
                                                         | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens3 !! 55)
                                                         | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens3 !! 56)
                                                         | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens3 !! 57)
                                                         | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens3 !! 58)
                                                         | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens3 !! 59)
                                                         | otherwise = Translate (saltaX x) (saltaY y) (imagens3 !! 39)  
criarJogador3 (Jogador (x,y)) t imagens3 (Move Direita)  | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens3 !! 60)
                                                         | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens3 !! 61)
                                                         | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens3 !! 62)
                                                         | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens3 !! 63)
                                                         | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens3 !! 64)
                                                         | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens3 !! 65)
                                                         | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens3 !! 66)
                                                         | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens3 !! 67)
                                                         | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens3 !! 68)
                                                         | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens3 !! 69)
                                                         | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens3 !! 70)
                                                         | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens3 !! 71)
                                                         | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens3 !! 72)
                                                         | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens3 !! 73)
                                                         | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens3 !! 74)
                                                         | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens3 !! 75)
                                                         | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens3 !! 76)
                                                         | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens3 !! 77)
                                                         | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens3 !! 78)
                                                         | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens3 !! 79)
                                                         | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens3 !! 80)
                                                         | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens3 !! 81)
                                                         | otherwise = Translate (saltaX x) (saltaY y) (imagens3 !! 60)


{-| Funcoes auxiliares que controlam o movimento do jogador no mapa, feitas com as medidas perfeitas para o jogador ficar sempre no centro do Mapa

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
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Sair_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Sair_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar Facil, jogo1, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Facil False Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaInstrucoes False Facil, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = error "Jogo Terminou"
-- Pagina Dificuldade
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Menu1 b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Media b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Facil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Media b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Dificil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Media b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Media b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Dificil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Menu1 b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Facil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Facil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar Facil, jogo1, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Media b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar Media, jogo1, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Dificil b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar Dificil, jogo1, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaDificuldade Menu1 b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) | b == True = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
                                                                                                                                            | otherwise = (PaginaPrincipal Dificuldades_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
-- Pagina controlos  
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaInstrucoes b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
                                                                                                                                  | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaInstrucoes b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
                                                                                                                                    | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaInstrucoes b d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) | b == True = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
                                                                                                                                     | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
-- Pagina Pausa  
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Menu_2 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPausa Continuar_1 d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Continuar_1 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Menu_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
-- Pagina MenuPausa 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Continuar_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa NovoJogo d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, jogo1 , imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Dificuldades_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Facil True d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaInstrucoes True d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = error "Terminou Jogo"
-- Pagina Perdeu jogo
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Reniciar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, jogo1, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaDificuldade Facil False d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Menu_3 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPrincipal Jogar, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
-- Pagina Jogar 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, Jogo (deslocafinal j (Move Cima) m) m, imagens, tempo, (Move Cima), imagens2, imagens3, imagens4)           
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, Jogo (deslocafinal j (Move Baixo) m) m, imagens, tempo, (Move Cima), imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyLeft) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, Jogo (deslocafinal j (Move Esquerda) m) m, imagens, tempo, (Move Esquerda), imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeyRight) Down _ _) (PaginaJogar d, Jogo j m, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaJogar d, Jogo (deslocafinal j (Move Direita) m) m, imagens, tempo, (Move Direita), imagens2, imagens3, imagens4)
event (EventKey (SpecialKey KeySpace) Down _ _) (PaginaJogar d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4) = (PaginaPausa Continuar_1 d, jogo, imagens, tempo, direccao, imagens2, imagens3, imagens4)
event _ s = s

getJogo :: Mundo -> Jogo 
getJogo (_, j, _, _, _, _, _, _)= j 

animajogo1 :: Jogo -> Int -> Jogo
animajogo1 j a | (mod a 3000) < 1 = animaJogo j Parado
               | otherwise = j

{-| A funcao 'reageTempo' registra a passagem do tempo e atualiza o mapa com a ajuda das auxiliares 'deslizaJogo' para deslizar o jogo e a 'animaJogo' para mover os 'Obstaculos' como 'Troncos' e 'Carros'

==codigo: 
@
reageTempo :: Float -> Mundo -> Mundo 
novoMundoReageTempo z (PaginaJogar, Jogo j m, imagens, t,e) = (PaginaJogar, (deslizaJogo ((round(t+z))*200) (animaJogo (Jogo j m) Parado)), imagens, (t+z))
@
-}

reageTempo :: Float -> Mundo -> Mundo 
reageTempo z (PaginaJogar Facil, Jogo j m, imagens, t, dr, imagens2, imagens3, imagens4) | jogoTerminou (Jogo j m) == True = (PaginaPerdeuJogo Reniciar Facil, Jogo j m, imagens, t, dr, imagens2, imagens3, imagens4)
                                                                               | otherwise =  (PaginaJogar Facil, Jogo j m{-(deslizaJogo ((round(t+z))*300) (animajogo1(Jogo j m) (round((t+z)*1000))))-}, imagens, (t+z), (Move Cima), imagens2, imagens3, imagens4)
reageTempo z (PaginaJogar Media, Jogo j m, imagens, t, dr, imagens2, imagens3, imagens4) | jogoTerminou (Jogo j m) == True = (PaginaPerdeuJogo Reniciar Media, Jogo j m, imagens, t, dr, imagens2, imagens3, imagens4)
                                                                               | otherwise =  (PaginaJogar Media, Jogo j m{-(deslizaJogo ((round(t+z))*300) (animajogo1(Jogo j m) (round((t+z)*1000))))-}, imagens, (t+z), (Move Cima), imagens2, imagens3, imagens4)
reageTempo z (PaginaJogar Dificil, Jogo j m, imagens, t, dr, imagens2, imagens3, imagens4) | jogoTerminou (Jogo j m) == True = (PaginaPerdeuJogo Reniciar Dificil, Jogo j m, imagens, t, dr, imagens2, imagens3, imagens4)
                                                                                 | otherwise =  (PaginaJogar Dificil, Jogo j m{-(deslizaJogo ((round(t+z))*300) (animajogo1(Jogo j m) (round((t+z)*1000))))-}, imagens, (t+z), (Move Cima), imagens2, imagens3, imagens4)
reageTempo z (PaginaPrincipal c, jogo, imagens, t, dr, imagens2, imagens3, imagens4) = (PaginaPrincipal c, jogo, imagens, (t+z),dr, imagens2, imagens3, imagens4)
reageTempo z (PaginaDificuldade d1 b d2, jogo, imagens, t, dr, imagens2, imagens3, imagens4) = (PaginaDificuldade d1 b d2, jogo, imagens, (t+z),dr, imagens2, imagens3, imagens4)
reageTempo z (PaginaInstrucoes b d, jogo, imagens, t, dr, imagens2, imagens3, imagens4) = (PaginaInstrucoes b d, jogo, imagens, (t+z),dr, imagens2, imagens3, imagens4)
reageTempo z (PaginaMenuPausa p d, jogo, imagens, t, dr, imagens2, imagens3, imagens4) = (PaginaMenuPausa p d, jogo, imagens, (t+z),dr, imagens2, imagens3, imagens4)
reageTempo z (PaginaPerdeuJogo c d, jogo, imagens, t, dr, imagens2, imagens3, imagens4) = (PaginaPerdeuJogo c d, jogo, imagens, (t+z),dr, imagens2, imagens3, imagens4)
reageTempo z (PaginaPausa c d, jogo, imagens, t, dr, imagens2, imagens3, imagens4) = (PaginaPausa c d, jogo, imagens, (t+z),dr, imagens2, imagens3, imagens4)
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
fr = 2
@
-}

fr :: Int
fr = 2

{-| Variavel 'cor' contem a cor do background do nosso programa

==codigo:
@
cor :: Color
cor = black
@ 
-}
cor :: Color
cor = black


carroselect1 = "./bmps/ESTILO"++estilo1++"/carro.bmp"
carroselect2 = "./bmps/ESTILO"++estilo2++"/carro.bmp"
carroselect3 = "./bmps/ESTILO"++estilo3++"/carro.bmp"
estilo1 = "1"
estilo2 = "2"
estilo3 = "3"


{-| Funcao 'main' e a funcao que contem todos os 'bmps' do jogo, e executa a funcao play, que vai carregar para o ecra todos os menus, e jogos, para que o user possa interagir com o programa

==codigo:
@

@
-}
main :: IO ()
main = do
         banner1_1 <- loadBMP "./bmps/banner.bmp"
         banner1_2 <- loadBMP "./bmps/banner.bmp" 
         banner1_3 <- loadBMP "./bmps/banner.bmp"
         galinha1_1 <- loadBMP "Chicken_JE2_BE2.bmp"
         galinha1_2 <- loadBMP "Chicken_JE2_BE2.bmp"
         galinha1_3 <- loadBMP "Chicken_JE2_BE2.bmp"
         rio1 <- loadBMP "bmps/ESTILO1/rio.bmp"
         relva1 <- loadBMP "./bmps/ESTILO1/relva.bmp"
         estrada1 <- loadBMP "./bmps/ESTILO1/estrada.bmp"
         arvore1 <- loadBMP "./bmps/ESTILO1/arvore.bmp"
         tronco1 <- loadBMP "./bmps/ESTILO1/tronco.bmp"
         carro1 <- loadBMP carroselect1         
         galinha2_1 <- loadBMP "Chicken_JE2_BE2.bmp"
         galinha2_2 <- loadBMP "Chicken_JE2_BE2.bmp"
         galinha2_3 <- loadBMP "Chicken_JE2_BE2.bmp"         
         rio2 <- loadBMP "bmps/ESTILO2/rio.bmp"
         estrada2 <-loadBMP "./bmps/ESTILO2/estrada.bmp"
         relva2 <- loadBMP "./bmps/ESTILO2/relva.bmp"
         arvore2 <- loadBMP "./bmps/ESTILO2/arvore2.bmp"
         tronco2 <- loadBMP "./bmps/ESTILO2/tronco.bmp"
         carro2 <- loadBMP carroselect2
         galinha3_1 <- loadBMP "Chicken_JE2_BE2.bmp"
         galinha3_2 <- loadBMP "Chicken_JE2_BE2.bmp"
         galinha3_3 <- loadBMP "Chicken_JE2_BE2.bmp" 
         rio3 <- loadBMP "./bmps/ESTILO3/rio.bmp"
         relva3 <- loadBMP "./bmps/ESTILO3/relva.bmp"
         estrada3 <- loadBMP "./bmps/ESTILO3/estrada.bmp"
         arvore3 <- loadBMP "./bmps/ESTILO3/arvore.bmp"
         tronco3 <- loadBMP "./bmps/ESTILO3/tronco.bmp"
         carro3 <- loadBMP carroselect3
         galinha_1Frente_1of39 <-  loadBMP "./bmps/ESTILO1/andar/0001.bmp"
         galinha_1Frente_2of39 <-  loadBMP "./bmps/ESTILO1/andar/0002.bmp"
         galinha_1Frente_3of39 <-  loadBMP "./bmps/ESTILO1/andar/0003.bmp"
         galinha_1Frente_4of39 <-  loadBMP "./bmps/ESTILO1/andar/0004.bmp"
         galinha_1Frente_5of39 <-  loadBMP "./bmps/ESTILO1/andar/0005.bmp"
         galinha_1Frente_6of39 <-  loadBMP "./bmps/ESTILO1/andar/0006.bmp"
         galinha_1Frente_7of39 <-  loadBMP "./bmps/ESTILO1/andar/0007.bmp"
         galinha_1Frente_8of39 <-  loadBMP "./bmps/ESTILO1/andar/0008.bmp"
         galinha_1Frente_9of39 <-  loadBMP "./bmps/ESTILO1/andar/0009.bmp"
         galinha_1Frente_10of39 <- loadBMP  "./bmps/ESTILO1/andar/0010.bmp"
         galinha_1Frente_11of39 <- loadBMP  "./bmps/ESTILO1/andar/0011.bmp"
         galinha_1Frente_12of39 <- loadBMP  "./bmps/ESTILO1/andar/0012.bmp"
         galinha_1Frente_13of39 <- loadBMP  "./bmps/ESTILO1/andar/0013.bmp"
         galinha_1Frente_14of39 <- loadBMP  "./bmps/ESTILO1/andar/0014.bmp"
         galinha_1Frente_15of39 <- loadBMP  "./bmps/ESTILO1/andar/0015.bmp"
         galinha_1Frente_16of39 <- loadBMP  "./bmps/ESTILO1/andar/0016.bmp"
         galinha_1Frente_17of39 <- loadBMP  "./bmps/ESTILO1/andar/0017.bmp"
         galinha_1Frente_18of39 <- loadBMP  "./bmps/ESTILO1/andar/0018.bmp"
         galinha_1Frente_19of39 <- loadBMP  "./bmps/ESTILO1/andar/0019.bmp"
         galinha_1Frente_20of39 <- loadBMP  "./bmps/ESTILO1/andar/0020.bmp"
         galinha_1Frente_21of39 <- loadBMP  "./bmps/ESTILO1/andar/0021.bmp"
         galinha_1Frente_22of39 <- loadBMP  "./bmps/ESTILO1/andar/0022.bmp"
         galinha_1Frente_23of39 <- loadBMP  "./bmps/ESTILO1/andar/0023.bmp"
         galinha_1Frente_24of39 <- loadBMP  "./bmps/ESTILO1/andar/0024.bmp"
         galinha_1Frente_25of39 <- loadBMP  "./bmps/ESTILO1/andar/0025.bmp"
         galinha_1Frente_26of39 <- loadBMP  "./bmps/ESTILO1/andar/0026.bmp"
         galinha_1Frente_27of39 <- loadBMP  "./bmps/ESTILO1/andar/0027.bmp"
         galinha_1Frente_28of39 <- loadBMP  "./bmps/ESTILO1/andar/0028.bmp"
         galinha_1Frente_29of39 <- loadBMP  "./bmps/ESTILO1/andar/0029.bmp"
         galinha_1Frente_30of39 <- loadBMP  "./bmps/ESTILO1/andar/0030.bmp"
         galinha_1Frente_31of39 <- loadBMP  "./bmps/ESTILO1/andar/0031.bmp"
         galinha_1Frente_32of39 <- loadBMP  "./bmps/ESTILO1/andar/0032.bmp"
         galinha_1Frente_33of39 <- loadBMP  "./bmps/ESTILO1/andar/0033.bmp"
         galinha_1Frente_34of39 <- loadBMP  "./bmps/ESTILO1/andar/0034.bmp"
         galinha_1Frente_35of39 <- loadBMP  "./bmps/ESTILO1/andar/0035.bmp"
         galinha_1Frente_36of39 <- loadBMP  "./bmps/ESTILO1/andar/0036.bmp"
         galinha_1Frente_37of39 <- loadBMP  "./bmps/ESTILO1/andar/0037.bmp"
         galinha_1Frente_38of39 <- loadBMP  "./bmps/ESTILO1/andar/0038.bmp"
         galinha_1Frente_39of39 <- loadBMP  "./bmps/ESTILO1/andar/0039.bmp"
         galinha_1Esquerda_1of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0001.bmp"
         galinha_1Esquerda_2of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0002.bmp"
         galinha_1Esquerda_3of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0003.bmp"
         galinha_1Esquerda_4of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0004.bmp"
         galinha_1Esquerda_5of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0005.bmp"
         galinha_1Esquerda_6of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0006.bmp"
         galinha_1Esquerda_7of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0007.bmp"
         galinha_1Esquerda_8of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0008.bmp"
         galinha_1Esquerda_9of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0009.bmp"
         galinha_1Esquerda_10of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0010.bmp"
         galinha_1Esquerda_11of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0011.bmp"
         galinha_1Esquerda_12of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0012.bmp"
         galinha_1Esquerda_14of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0014.bmp"
         galinha_1Esquerda_15of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0015.bmp"
         galinha_1Esquerda_16of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0016.bmp"
         galinha_1Esquerda_17of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0017.bmp"
         galinha_1Esquerda_18of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0018.bmp"
         galinha_1Esquerda_19of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0019.bmp"
         galinha_1Esquerda_20of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0020.bmp"
         galinha_1Esquerda_21of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0021.bmp"
         galinha_1Esquerda_22of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0022.bmp"
         galinha_1Esquerda_23of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0023.bmp"
         galinha_1Esquerda_24of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0024.bmp"
         galinha_1Esquerda_25of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0025.bmp"
         galinha_1Esquerda_26of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0026.bmp"
         galinha_1Esquerda_27of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0027.bmp"
         galinha_1Esquerda_28of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0028.bmp"
         galinha_1Esquerda_29of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0029.bmp"
         galinha_1Esquerda_30of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0030.bmp"
         galinha_1Esquerda_31of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0031.bmp"
         galinha_1Esquerda_32of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0032.bmp"
         galinha_1Esquerda_33of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0033.bmp"
         galinha_1Esquerda_34of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0034.bmp"
         galinha_1Esquerda_35of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0035.bmp"
         galinha_1Esquerda_36of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0036.bmp"
         galinha_1Esquerda_37of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0037.bmp"
         galinha_1Esquerda_38of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0038.bmp"
         galinha_1Esquerda_39of39 <- loadBMP  "./bmps/ESTILO1/esquerda/0039.bmp"
         galinha_1Direita_1of39 <- loadBMP  "./bmps/ESTILO1/direita/0001.bmp"
         galinha_1Direita_2of39 <- loadBMP  "./bmps/ESTILO1/direita/0002.bmp"
         galinha_1Direita_3of39 <- loadBMP  "./bmps/ESTILO1/direita/0003.bmp"
         galinha_1Direita_4of39 <- loadBMP  "./bmps/ESTILO1/direita/0004.bmp"
         galinha_1Direita_5of39 <- loadBMP  "./bmps/ESTILO1/direita/0005.bmp"
         galinha_1Direita_6of39 <- loadBMP  "./bmps/ESTILO1/direita/0006.bmp"
         galinha_1Direita_7of39 <- loadBMP  "./bmps/ESTILO1/direita/0007.bmp"
         galinha_1Direita_8of39 <- loadBMP  "./bmps/ESTILO1/direita/0008.bmp"
         galinha_1Direita_9of39 <- loadBMP  "./bmps/ESTILO1/direita/0009.bmp"
         galinha_1Direita_10of39 <- loadBMP  "./bmps/ESTILO1/direita/0010.bmp"
         galinha_1Direita_11of39 <- loadBMP  "./bmps/ESTILO1/direita/0011.bmp"
         galinha_1Direita_12of39 <- loadBMP  "./bmps/ESTILO1/direita/0012.bmp"
         galinha_1Direita_14of39 <- loadBMP  "./bmps/ESTILO1/direita/0014.bmp"
         galinha_1Direita_15of39 <- loadBMP  "./bmps/ESTILO1/direita/0015.bmp"
         galinha_1Direita_16of39 <- loadBMP  "./bmps/ESTILO1/direita/0016.bmp"
         galinha_1Direita_17of39 <- loadBMP  "./bmps/ESTILO1/direita/0017.bmp"
         galinha_1Direita_18of39 <- loadBMP  "./bmps/ESTILO1/direita/0018.bmp"
         galinha_1Direita_19of39 <- loadBMP  "./bmps/ESTILO1/direita/0019.bmp"
         galinha_1Direita_20of39 <- loadBMP  "./bmps/ESTILO1/direita/0020.bmp"
         galinha_1Direita_21of39 <- loadBMP  "./bmps/ESTILO1/direita/0021.bmp"
         galinha_1Direita_22of39 <- loadBMP  "./bmps/ESTILO1/direita/0022.bmp"
         galinha_1Direita_23of39 <- loadBMP  "./bmps/ESTILO1/direita/0023.bmp"
         galinha_1Direita_24of39 <- loadBMP  "./bmps/ESTILO1/direita/0024.bmp"
         galinha_1Direita_25of39 <- loadBMP  "./bmps/ESTILO1/direita/0025.bmp"
         galinha_1Direita_26of39 <- loadBMP  "./bmps/ESTILO1/direita/0026.bmp"
         galinha_1Direita_27of39 <- loadBMP  "./bmps/ESTILO1/direita/0027.bmp"
         galinha_1Direita_28of39 <- loadBMP  "./bmps/ESTILO1/direita/0028.bmp"
         galinha_1Direita_29of39 <- loadBMP  "./bmps/ESTILO1/direita/0029.bmp"
         galinha_1Direita_30of39 <- loadBMP  "./bmps/ESTILO1/direita/0030.bmp"
         galinha_1Direita_31of39 <- loadBMP  "./bmps/ESTILO1/direita/0031.bmp"
         galinha_1Direita_32of39 <- loadBMP  "./bmps/ESTILO1/direita/0032.bmp"
         galinha_1Direita_33of39 <- loadBMP  "./bmps/ESTILO1/direita/0033.bmp"
         galinha_1Direita_34of39 <- loadBMP  "./bmps/ESTILO1/direita/0034.bmp"
         galinha_1Direita_35of39 <- loadBMP  "./bmps/ESTILO1/direita/0035.bmp"
         galinha_1Direita_36of39 <- loadBMP  "./bmps/ESTILO1/direita/0036.bmp"
         galinha_1Direita_37of39 <- loadBMP  "./bmps/ESTILO1/direita/0037.bmp"
         galinha_1Direita_38of39 <- loadBMP  "./bmps/ESTILO1/direita/0038.bmp"
         galinha_1Direita_39of39 <- loadBMP  "./bmps/ESTILO1/direita/0039.bmp"
         galinha_2Frente_1of39 <-  loadBMP "./bmps/ESTILO2/andar/0001.bmp"
         galinha_2Frente_2of39 <-  loadBMP "./bmps/ESTILO2/andar/0002.bmp"
         galinha_2Frente_3of39 <-  loadBMP "./bmps/ESTILO2/andar/0003.bmp"
         galinha_2Frente_4of39 <-  loadBMP "./bmps/ESTILO2/andar/0004.bmp"
         galinha_2Frente_5of39 <-  loadBMP "./bmps/ESTILO2/andar/0005.bmp"
         galinha_2Frente_6of39 <-  loadBMP "./bmps/ESTILO2/andar/0006.bmp"
         galinha_2Frente_7of39 <-  loadBMP "./bmps/ESTILO2/andar/0007.bmp"
         galinha_2Frente_8of39 <-  loadBMP "./bmps/ESTILO2/andar/0008.bmp"
         galinha_2Frente_9of39 <-  loadBMP "./bmps/ESTILO2/andar/0009.bmp"
         galinha_2Frente_10of39 <- loadBMP  "./bmps/ESTILO2/andar/0010.bmp"
         galinha_2Frente_11of39 <- loadBMP  "./bmps/ESTILO2/andar/0011.bmp"
         galinha_2Frente_12of39 <- loadBMP  "./bmps/ESTILO2/andar/0012.bmp"
         galinha_2Frente_13of39 <- loadBMP  "./bmps/ESTILO2/andar/0013.bmp"
         galinha_2Frente_14of39 <- loadBMP  "./bmps/ESTILO2/andar/0014.bmp"
         galinha_2Frente_15of39 <- loadBMP  "./bmps/ESTILO2/andar/0015.bmp"
         galinha_2Frente_16of39 <- loadBMP  "./bmps/ESTILO2/andar/0016.bmp"
         galinha_2Frente_17of39 <- loadBMP  "./bmps/ESTILO2/andar/0017.bmp"
         galinha_2Frente_18of39 <- loadBMP  "./bmps/ESTILO2/andar/0018.bmp"
         galinha_2Frente_19of39 <- loadBMP  "./bmps/ESTILO2/andar/0019.bmp"
         galinha_2Frente_20of39 <- loadBMP  "./bmps/ESTILO2/andar/0020.bmp"
         galinha_2Frente_21of39 <- loadBMP  "./bmps/ESTILO2/andar/0021.bmp"
         galinha_2Frente_22of39 <- loadBMP  "./bmps/ESTILO2/andar/0022.bmp"
         galinha_2Frente_23of39 <- loadBMP  "./bmps/ESTILO2/andar/0023.bmp"
         galinha_2Frente_24of39 <- loadBMP  "./bmps/ESTILO2/andar/0024.bmp"
         galinha_2Frente_25of39 <- loadBMP  "./bmps/ESTILO2/andar/0025.bmp"
         galinha_2Frente_26of39 <- loadBMP  "./bmps/ESTILO2/andar/0026.bmp"
         galinha_2Frente_27of39 <- loadBMP  "./bmps/ESTILO2/andar/0027.bmp"
         galinha_2Frente_28of39 <- loadBMP  "./bmps/ESTILO2/andar/0028.bmp"
         galinha_2Frente_29of39 <- loadBMP  "./bmps/ESTILO2/andar/0029.bmp"
         galinha_2Frente_30of39 <- loadBMP  "./bmps/ESTILO2/andar/0030.bmp"
         galinha_2Frente_31of39 <- loadBMP  "./bmps/ESTILO2/andar/0031.bmp"
         galinha_2Frente_32of39 <- loadBMP  "./bmps/ESTILO2/andar/0032.bmp"
         galinha_2Frente_33of39 <- loadBMP  "./bmps/ESTILO2/andar/0033.bmp"
         galinha_2Frente_34of39 <- loadBMP  "./bmps/ESTILO2/andar/0034.bmp"
         galinha_2Frente_35of39 <- loadBMP  "./bmps/ESTILO2/andar/0035.bmp"
         galinha_2Frente_36of39 <- loadBMP  "./bmps/ESTILO2/andar/0036.bmp"
         galinha_2Frente_37of39 <- loadBMP  "./bmps/ESTILO2/andar/0037.bmp"
         galinha_2Frente_38of39 <- loadBMP  "./bmps/ESTILO2/andar/0038.bmp"
         galinha_2Frente_39of39 <- loadBMP  "./bmps/ESTILO2/andar/0039.bmp"
         galinha_2Esquerda_10of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0010.bmp"
         galinha_2Esquerda_11of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0011.bmp"
         galinha_2Esquerda_12of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0012.bmp"
         galinha_2Esquerda_14of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0014.bmp"
         galinha_2Esquerda_15of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0015.bmp"
         galinha_2Esquerda_16of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0016.bmp"
         galinha_2Esquerda_17of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0017.bmp"
         galinha_2Esquerda_18of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0018.bmp"
         galinha_2Esquerda_19of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0019.bmp"
         galinha_2Esquerda_20of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0020.bmp"
         galinha_2Esquerda_21of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0021.bmp"
         galinha_2Esquerda_22of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0022.bmp"
         galinha_2Esquerda_23of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0023.bmp"
         galinha_2Esquerda_24of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0024.bmp"
         galinha_2Esquerda_25of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0025.bmp"
         galinha_2Esquerda_26of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0026.bmp"
         galinha_2Esquerda_27of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0027.bmp"
         galinha_2Esquerda_28of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0028.bmp"
         galinha_2Esquerda_29of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0029.bmp"
         galinha_2Esquerda_30of39 <- loadBMP  "./bmps/ESTILO2/ESQUERDA/0030.bmp"
         galinha_1Direita_10of39 <- loadBMP  "./bmps/ESTILO2/direita/0010.bmp"
         galinha_1Direita_11of39 <- loadBMP  "./bmps/ESTILO2/direita/0011.bmp"
         galinha_1Direita_12of39 <- loadBMP  "./bmps/ESTILO2/direita/0012.bmp"
         galinha_1Direita_14of39 <- loadBMP  "./bmps/ESTILO2/direita/0014.bmp"
         galinha_1Direita_15of39 <- loadBMP  "./bmps/ESTILO2/direita/0015.bmp"
         galinha_1Direita_16of39 <- loadBMP  "./bmps/ESTILO2/direita/0016.bmp"
         galinha_1Direita_17of39 <- loadBMP  "./bmps/ESTILO2/direita/0017.bmp"
         galinha_1Direita_18of39 <- loadBMP  "./bmps/ESTILO2/direita/0018.bmp"
         galinha_1Direita_19of39 <- loadBMP  "./bmps/ESTILO2/direita/0019.bmp"
         galinha_1Direita_20of39 <- loadBMP  "./bmps/ESTILO2/direita/0020.bmp"
         galinha_1Direita_21of39 <- loadBMP  "./bmps/ESTILO2/direita/0021.bmp"
         galinha_1Direita_22of39 <- loadBMP  "./bmps/ESTILO2/direita/0022.bmp"
         galinha_1Direita_23of39 <- loadBMP  "./bmps/ESTILO2/direita/0023.bmp"
         galinha_1Direita_24of39 <- loadBMP  "./bmps/ESTILO2/direita/0024.bmp"
         galinha_1Direita_25of39 <- loadBMP  "./bmps/ESTILO2/direita/0025.bmp"
         galinha_1Direita_26of39 <- loadBMP  "./bmps/ESTILO2/direita/0026.bmp"
         galinha_1Direita_27of39 <- loadBMP  "./bmps/ESTILO2/direita/0027.bmp"
         galinha_1Direita_28of39 <- loadBMP  "./bmps/ESTILO2/direita/0028.bmp"
         galinha_1Direita_29of39 <- loadBMP  "./bmps/ESTILO2/direita/0029.bmp"
         galinha_1Direita_30of39 <- loadBMP  "./bmps/ESTILO2/direita/0030.bmp"
         galinha_3Frente_1of39 <-  loadBMP "./bmps/ESTILO3/andar/0001.bmp"
         galinha_3Frente_2of39 <-  loadBMP "./bmps/ESTILO3/andar/0002.bmp"
         galinha_3Frente_3of39 <-  loadBMP "./bmps/ESTILO3/andar/0003.bmp"
         galinha_3Frente_4of39 <-  loadBMP "./bmps/ESTILO3/andar/0004.bmp"
         galinha_3Frente_5of39 <-  loadBMP "./bmps/ESTILO3/andar/0005.bmp"
         galinha_3Frente_6of39 <-  loadBMP "./bmps/ESTILO3/andar/0006.bmp"
         galinha_3Frente_7of39 <-  loadBMP "./bmps/ESTILO3/andar/0007.bmp"
         galinha_3Frente_8of39 <-  loadBMP "./bmps/ESTILO3/andar/0008.bmp"
         galinha_3Frente_9of39 <-  loadBMP "./bmps/ESTILO3/andar/0009.bmp"
         galinha_3Frente_10of39 <- loadBMP  "./bmps/ESTILO3/andar/0010.bmp"
         galinha_3Frente_11of39 <- loadBMP  "./bmps/ESTILO3/andar/0011.bmp"
         galinha_3Frente_12of39 <- loadBMP  "./bmps/ESTILO3/andar/0012.bmp"
         galinha_3Frente_13of39 <- loadBMP  "./bmps/ESTILO3/andar/0013.bmp"
         galinha_3Frente_14of39 <- loadBMP  "./bmps/ESTILO3/andar/0014.bmp"
         galinha_3Frente_15of39 <- loadBMP  "./bmps/ESTILO3/andar/0015.bmp"
         galinha_3Frente_16of39 <- loadBMP  "./bmps/ESTILO3/andar/0016.bmp"
         galinha_3Frente_17of39 <- loadBMP  "./bmps/ESTILO3/andar/0017.bmp"
         galinha_3Frente_18of39 <- loadBMP  "./bmps/ESTILO3/andar/0018.bmp"
         galinha_3Frente_19of39 <- loadBMP  "./bmps/ESTILO3/andar/0019.bmp"
         galinha_3Frente_20of39 <- loadBMP  "./bmps/ESTILO3/andar/0020.bmp"
         galinha_3Frente_21of39 <- loadBMP  "./bmps/ESTILO3/andar/0021.bmp"
         galinha_3Frente_22of39 <- loadBMP  "./bmps/ESTILO3/andar/0022.bmp"
         galinha_3Frente_23of39 <- loadBMP  "./bmps/ESTILO3/andar/0023.bmp"
         galinha_3Frente_24of39 <- loadBMP  "./bmps/ESTILO3/andar/0024.bmp"
         galinha_3Frente_25of39 <- loadBMP  "./bmps/ESTILO3/andar/0025.bmp"
         galinha_3Frente_26of39 <- loadBMP  "./bmps/ESTILO3/andar/0026.bmp"
         galinha_3Frente_27of39 <- loadBMP  "./bmps/ESTILO3/andar/0027.bmp"
         galinha_3Frente_28of39 <- loadBMP  "./bmps/ESTILO3/andar/0028.bmp"
         galinha_3Frente_29of39 <- loadBMP  "./bmps/ESTILO3/andar/0029.bmp"
         galinha_3Frente_30of39 <- loadBMP  "./bmps/ESTILO3/andar/0030.bmp"
         galinha_3Frente_31of39 <- loadBMP  "./bmps/ESTILO3/andar/0031.bmp"
         galinha_3Frente_32of39 <- loadBMP  "./bmps/ESTILO3/andar/0032.bmp"
         galinha_3Frente_33of39 <- loadBMP  "./bmps/ESTILO3/andar/0033.bmp"
         galinha_3Frente_34of39 <- loadBMP  "./bmps/ESTILO3/andar/0034.bmp"
         galinha_3Frente_35of39 <- loadBMP  "./bmps/ESTILO3/andar/0035.bmp"
         galinha_3Frente_36of39 <- loadBMP  "./bmps/ESTILO3/andar/0036.bmp"
         galinha_3Frente_37of39 <- loadBMP  "./bmps/ESTILO3/andar/0037.bmp"
         galinha_3Frente_38of39 <- loadBMP  "./bmps/ESTILO3/andar/0038.bmp"
         galinha_3Frente_39of39 <- loadBMP  "./bmps/ESTILO3/andar/0039.bmp"
         galinha_3Esquerda_10of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0010.bmp"
         galinha_3Esquerda_11of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0011.bmp"
         galinha_3Esquerda_12of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0012.bmp"
         galinha_3Esquerda_14of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0014.bmp"
         galinha_3Esquerda_15of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0015.bmp"
         galinha_3Esquerda_16of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0016.bmp"
         galinha_3Esquerda_17of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0017.bmp"
         galinha_3Esquerda_18of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0018.bmp"
         galinha_3Esquerda_19of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0019.bmp"
         galinha_3Esquerda_20of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0020.bmp"
         galinha_3Esquerda_21of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0021.bmp"
         galinha_3Esquerda_22of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0022.bmp"
         galinha_3Esquerda_23of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0023.bmp"
         galinha_3Esquerda_24of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0024.bmp"
         galinha_3Esquerda_25of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0025.bmp"
         galinha_3Esquerda_26of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0026.bmp"
         galinha_3Esquerda_27of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0027.bmp"
         galinha_3Esquerda_28of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0028.bmp"
         galinha_3Esquerda_29of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0029.bmp"
         galinha_3Esquerda_30of39 <- loadBMP  "./bmps/ESTILO3/esquerda/0030.bmp"
         galinha_3Direita_10of39 <- loadBMP  "./bmps/ESTILO3/direita/0010.bmp"
         galinha_3Direita_11of39 <- loadBMP  "./bmps/ESTILO3/direita/0011.bmp"
         galinha_3Direita_12of39 <- loadBMP  "./bmps/ESTILO3/direita/0012.bmp"
         galinha_3Direita_14of39 <- loadBMP  "./bmps/ESTILO3/direita/0014.bmp"
         galinha_3Direita_15of39 <- loadBMP  "./bmps/ESTILO3/direita/0015.bmp"
         galinha_3Direita_16of39 <- loadBMP  "./bmps/ESTILO3/direita/0016.bmp"
         galinha_3Direita_17of39 <- loadBMP  "./bmps/ESTILO3/direita/0017.bmp"
         galinha_3Direita_18of39 <- loadBMP  "./bmps/ESTILO3/direita/0018.bmp"
         galinha_3Direita_19of39 <- loadBMP  "./bmps/ESTILO3/direita/0019.bmp"
         galinha_3Direita_20of39 <- loadBMP  "./bmps/ESTILO3/direita/0020.bmp"
         galinha_3Direita_21of39 <- loadBMP  "./bmps/ESTILO3/direita/0021.bmp"
         galinha_3Direita_22of39 <- loadBMP  "./bmps/ESTILO3/direita/0022.bmp"
         galinha_3Direita_23of39 <- loadBMP  "./bmps/ESTILO3/direita/0023.bmp"
         galinha_3Direita_24of39 <- loadBMP  "./bmps/ESTILO3/direita/0024.bmp"
         galinha_3Direita_25of39 <- loadBMP  "./bmps/ESTILO3/direita/0025.bmp"
         galinha_3Direita_26of39 <- loadBMP  "./bmps/ESTILO3/direita/0026.bmp"
         galinha_3Direita_27of39 <- loadBMP  "./bmps/ESTILO3/direita/0027.bmp"
         galinha_3Direita_28of39 <- loadBMP  "./bmps/ESTILO3/direita/0028.bmp"
         galinha_3Direita_29of39 <- loadBMP  "./bmps/ESTILO3/direita/0029.bmp"
         galinha_3Direita_30of39 <- loadBMP  "./bmps/ESTILO3/direita/0030.bmp"
         banner1of20 <- loadBMP "./bmps/banner/0001.bmp"
         banner2of20 <- loadBMP "./bmps/banner/0002.bmp"
         banner3of20 <- loadBMP "./bmps/banner/0003.bmp"
         banner4of20 <- loadBMP "./bmps/banner/0004.bmp"
         banner5of20 <- loadBMP "./bmps/banner/0005.bmp"
         banner6of20 <- loadBMP "./bmps/banner/0006.bmp"
         banner7of20 <- loadBMP "./bmps/banner/0007.bmp"
         banner8of20 <- loadBMP "./bmps/banner/0008.bmp"
         banner9of20 <- loadBMP "./bmps/banner/0009.bmp"
         banner10of20 <- loadBMP "./bmps/banner/0010.bmp"
         banner11of20 <- loadBMP "./bmps/banner/0011.bmp"
         banner12of20 <- loadBMP "./bmps/banner/0012.bmp"
         banner13of20 <- loadBMP "./bmps/banner/0013.bmp"
         banner14of20 <- loadBMP "./bmps/banner/0014.bmp"
         banner15of20 <- loadBMP "./bmps/banner/0015.bmp"
         banner16of20 <- loadBMP "./bmps/banner/0016.bmp"
         banner17of20 <- loadBMP "./bmps/banner/0017.bmp"
         banner18of20 <- loadBMP "./bmps/banner/0018.bmp"
         banner19of20 <- loadBMP "./bmps/banner/0019.bmp"
         banner20of20 <- loadBMP "./bmps/banner/0020.bmp"
         banner21of20 <- loadBMP "./bmps/banner/0021.bmp"
         banner22of20 <- loadBMP "./bmps/banner/0022.bmp"
         banner23of20 <- loadBMP "./bmps/banner/0023.bmp"
         banner24of20 <- loadBMP "./bmps/banner/0024.bmp"
         banner25of20 <- loadBMP "./bmps/banner/0025.bmp"
         banner26of20 <- loadBMP "./bmps/banner/0026.bmp"
         banner27of20 <- loadBMP "./bmps/banner/0027.bmp"
         banner28of20 <- loadBMP "./bmps/banner/0028.bmp"
         banner29of20 <- loadBMP "./bmps/banner/0029.bmp"
         banner30of20 <- loadBMP "./bmps/banner/0030.bmp"
         banner31of20 <- loadBMP "./bmps/banner/0031.bmp"
         banner32of20 <- loadBMP "./bmps/banner/0032.bmp"
         banner33of20 <- loadBMP "./bmps/banner/0033.bmp"
         banner34of20 <- loadBMP "./bmps/banner/0034.bmp"
         banner35of20 <- loadBMP "./bmps/banner/0035.bmp"
         banner36of20 <- loadBMP "./bmps/banner/0036.bmp"
         banner37of20 <- loadBMP "./bmps/banner/0037.bmp"
         banner38of20 <- loadBMP "./bmps/banner/0038.bmp"
         banner39of20 <- loadBMP "./bmps/banner/0039.bmp"
         botao1 <- loadBMP "./bmps/botoes/cantinuarnao.bmp"
         botao2 <- loadBMP "./bmps/botoes/continuarsim.bmp"
         botao3 <- loadBMP "./bmps/botoes/dificilnao.bmp"
         botao4 <- loadBMP "./bmps/botoes/dificilsim.bmp"
         botao5 <- loadBMP "./bmps/botoes/difinao.bmp"
         botao6 <- loadBMP "./bmps/botoes/difisim.bmp"
         botao7 <- loadBMP "./bmps/botoes/facilnao.bmp"
         botao8 <- loadBMP "./bmps/botoes/facilsim.bmp"
         botao9 <- loadBMP "./bmps/botoes/Instruçõesnao.bmp"
         botao10 <- loadBMP "./bmps/botoes/Instruçõessim.bmp"
         botao11 <- loadBMP "./bmps/botoes/jogarnao.bmp"
         botao12 <- loadBMP "./bmps/botoes/jogarsim.bmp"
         botao13 <- loadBMP "./bmps/botoes/medianao.bmp"
         botao14 <- loadBMP "./bmps/botoes/mediasim.bmp"
         botao15 <- loadBMP "./bmps/botoes/menunao.bmp"
         botao16 <- loadBMP "./bmps/botoes/menusim.bmp"
         botao17 <- loadBMP "./bmps/botoes/novojogonao.bmp"
         botao18 <- loadBMP "./bmps/botoes/novojogosim.bmp"
         botao19 <- loadBMP "./bmps/botoes/sairnao.bmp"
         botao20 <- loadBMP "./bmps/botoes/sairsim.bmp"
         instructions <- loadBMP "./bmps/instruçoes.bmp"


         let imagens = [galinha1_1,galinha1_2,galinha1_3, scale 0.12 0.24 $ rio1, scale 0.12 0.24 $ relva1, scale 0.12 0.24 $ estrada1,galinha2_1,galinha2_2,galinha2_3, scale 0.12 0.24 $ rio2, scale 0.12 0.24 $ estrada2, scale 0.12 0.24 $ relva2, Translate 0.0 80.0 $ scale 0.12 0.24 $ arvore2, Translate 0.0 30.0 $ scale 0.12 0.24 $ tronco2, Translate 0.0 30.0 $ scale 0.12 0.24 $ carro2,galinha3_1,galinha3_2,galinha3_3, scale 0.12 0.24 $ rio3, scale 0.12 0.24 $ relva3, scale 0.12 0.24 $ estrada3, Translate 0.0 30.0 $ scale 0.12 0.24 $ carro3, Translate 0.0 30.0 $ scale 0.12 0.24 $ carro1, Translate 0.0 90.0 $ scale 0.12 0.24 $ arvore1, Translate 0.0 80.0 $ scale 0.12 0.24 $ arvore3, Translate 0.0 30.0 $ scale 0.12 0.24 $ tronco1, Translate 0.0 30.0 $ scale 0.12 0.24 $ tronco3, banner1_1, banner1_2, banner1_3, nenhum28, Translate 0.0 25.0 galinha28, Translate 0.0 25.0 galinha2, Translate 0.0 25.0 galinha3, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_1of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_2of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_3of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_4of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_5of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_6of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_7of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_8of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_9of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_10of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_12of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_13of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_17of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_22of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_27of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_30of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_31of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_32of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_33of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_34of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_35of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_36of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_37of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_38of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Frente_39of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_1of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_2of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_3of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_4of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_5of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_6of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_7of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_8of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_9of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_10of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_12of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_17of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_22of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_27of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_30of39, Translate 0.0 40.0 $ scale 0.08 0.16 $  galinha_1Esquerda_31of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_32of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_33of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_34of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_35of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_36of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_37of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_38of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Esquerda_39of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_1of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_2of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_3of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_4of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_5of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_6of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_7of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_8of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_9of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_10of39,Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_12of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_17of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_22of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_27of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_30of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_31of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_32of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_33of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_34of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_35of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_36of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_37of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_38of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_39of39, botao1, botao2, botao3, botao4, botao5, botao6, botao7, botao8, botao9, botao10, botao11, botao12, botao13, botao14, botao15, botao16, botao17, botao18, botao19, botao20]
        
         let imagens2 = [ Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_1of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_2of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_3of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_4of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_5of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_6of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_7of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_8of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_9of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_10of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_12of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_13of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_17of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_22of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_27of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_30of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_31of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_32of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_33of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_34of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_35of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_36of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_37of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_38of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Frente_39of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_10of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_12of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_17of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_22of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_27of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_2Esquerda_30of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_10of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_12of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_17of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_22of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_27of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Direita_30of39]

         let imagens3 = [ Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_1of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_2of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_3of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_4of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_5of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_6of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_7of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_8of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_9of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_10of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_12of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_13of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_17of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_22of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_27of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_30of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_31of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_32of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_33of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_34of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_35of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_36of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_37of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_38of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Frente_39of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_10of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_11of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_12of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_14of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_15of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_16of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_17of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_18of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_19of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_20of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_21of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_22of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_23of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_24of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_25of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_26of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_27of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_28of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_29of39, Translate 0.0 40.0 $ scale (-0.08) 0.16 $ galinha_3Esquerda_30of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_10of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_11of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_12of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_14of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_15of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_16of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_17of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_18of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_19of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_20of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_21of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_22of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_23of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_24of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_25of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_26of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_27of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_28of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_29of39, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_3Direita_30of39] 
         
         let imagens4 = [scale 1.0 0.8 banner1of20,scale 1.0 0.8 banner2of20, scale 1.0 0.8 banner3of20,scale 1.0 0.8 banner4of20,scale 1.0 0.8 banner5of20,scale 1.0 0.8 banner6of20,scale 1.0 0.8 banner7of20, scale 1.0 0.8 banner8of20, scale 1.0 0.8 banner9of20, scale 1.0 0.8 banner10of20, scale 1.0 0.8 banner11of20, scale 1.0 0.8 banner12of20, scale 1.0 0.8 banner13of20, scale 1.0 0.8 banner14of20, scale 1.0 0.8 banner15of20, scale 1.0 0.8 banner16of20, scale 1.0 0.8 banner17of20, scale 1.0 0.8 banner18of20, scale 1.0 0.8 banner19of20, scale 1.0 0.8 banner20of20, scale 1.0 0.8 banner21of20, scale 1.0 0.8 banner22of20, scale 1.0 0.8 banner23of20, scale 1.0 0.8 banner24of20, scale 1.0 0.8 banner25of20, scale 1.0 0.8 banner26of20, scale 1.0 0.8 banner27of20, scale 1.0 0.8 banner28of20, scale 1.0 0.8 banner29of20, scale 1.0 0.8 banner30of20, scale 1.0 0.8 banner31of20, scale 1.0 0.8 banner32of20, scale 1.0 0.8 banner33of20, scale 1.0 0.8 banner34of20, scale 1.0 0.8 banner35of20, scale 1.0 0.8 banner36of20, scale 1.0 0.8 banner37of20, scale 1.0 0.8 banner38of20, scale 1.0 0.8 banner39of20, scale 0.3 0.2 $ botao1, scale 0.3 0.2 $ botao2, scale 0.3 0.2 $ botao3, scale 0.3 0.2 $ botao4, scale 0.3 0.2 $ botao5, scale 0.3 0.2 $ botao6, scale 0.3 0.2 $ botao7, scale 0.3 0.2 $ botao8, scale 0.3 0.2 $ botao9, scale 0.3 0.2 $ botao10, scale 0.3 0.2 $ botao11, scale 0.3 0.2 $ botao12, scale 0.3 0.2 $ botao13, scale 0.3 0.2 $ botao14, scale 0.3 0.2 $ botao15, scale 0.3 0.2 $ botao16, scale 0.3 0.2 $ botao17, scale 0.3 0.2 $ botao18, scale 0.3 0.2 $ botao19, scale 0.3 0.2 $ botao20, scale 0.3 0.2 $ instructions]

         let tempo = 0.0 
         
         let direccao = (Move Cima)       
         play window cor fr (estadoInicial imagens tempo direccao imagens2 imagens3 imagens4) desenhaMundo event reageTempo

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