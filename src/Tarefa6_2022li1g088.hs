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

==codigo:
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
desenhaMundo (PaginaMenuPausa Instrucoes_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa Sair_2 d, jogo, imagens, tempo) = Pictures [Scale 1.0 1.0 (imagens !! 4)]
--PaginaJogar 
desenhaMundo (PaginaJogar, jogo, imagens, tempo) = Translate (-630) (370) $ scale 1.91 0.68 $ Pictures world28 
 where 
     world28 = desenhaTerrenos ++ desenhaObstaculos ++ [scoore] ++ [desenhajogador]
     desenhaTerrenos = criarTerreno p o (getLargura(getMapa (PaginaJogar, jogo, imagens, tempo))) (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens
     desenhaObstaculos = criarObstaculos p o (getTerreno(getMapa (PaginaJogar, jogo, imagens, tempo))) imagens 
     desenhajogador = criarJogador (getJogador (PaginaJogar, jogo, imagens, tempo)) (getTempo (PaginaJogar, jogo, imagens, tempo)) imagens
     scoore = mostrarTempo (getTempo(PaginaJogar, jogo, imagens, tempo)) 

{-| A funcao 'mostraTempo' mostra o tempo a ser registrado no jogo a passar

==codigo:
@
mostrarTempo:: Float -> Picture
mostrarTempo t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)
@
-}

mostrarTempo:: Float -> Picture
mostrarTempo t =  Translate 630 (-80) $ scale 0.2 0.2 $ Text (show $ round t)

{-| A funcao 'imageFundo' faz alternar as imagens do background do jogo

==codigo:
@
imagefundo :: Float -> Imagens -> Picture 
imagefundo t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                     | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                     | otherwise = (imagens !! 12)
@
-}

imagefundo :: Float -> Imagens -> Picture 
imagefundo t imagens | (mod (round (t*1000)) 300) < 100 = (imagens !! 4)
                     | (mod (round (t*1000)) 300) > 200 = (imagens !! 11)
                     | otherwise = (imagens !! 12)

{-| A funcao 'getImage' busca a imagem no 'Mundo'

==codigo: 
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

==codigo:
@
getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _, _) = m
@ 
-}

getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _, _) = m 

{-| A  funcao 'getLargura' busca a largura no 'Mapa'

==codigo:
@
getLargura :: Mapa -> Int 
getLargura (Mapa l ((te,obs):xs)) = l
@
-}

getLargura :: Mapa -> Int 
getLargura (Mapa l ((te,obs):xs)) = l

{-| A funcao 'getTerreno' busca o '[(Terreno,[Obstaculo])]' no 'Mapa'

==codigo:
@
getTerreno :: Mapa -> [(Terreno,[Obstaculo])] 
getTerreno (Mapa l ((te,obs):xs)) = ((te,obs):xs)
@
-}

getTerreno :: Mapa -> [(Terreno,[Obstaculo])] 
getTerreno (Mapa l ((te,obs):xs)) = ((te,obs):xs)


{-| A Funcao 'getJogador' extrai o 'Jogador' do 'Mundo'

==codigo:
@
getJogador :: Mundo -> Jogador
getJogador (_, Jogo j m, _, _) = j
@
-}

getJogador :: Mundo -> Jogador
getJogador (_, Jogo j m, _, _) = j

{-| O 'p' guarda o valor do x onde o Mapa vai comecar

==codigo:
@
p :: Float
p = 0.0
@
-}

p :: Float 
p = 0.0

{-|O 'o' guarda o Valor do y onde o Mapa vai comecar

==codigo:
@
o :: Float
o = 0.0
@
-}

o :: Float 
o = 0.0

{-| O 'lado' guarda o valor do lado da imagem, usado para contruir as figuras seguintes uma apos a outra e usado para controir as linhas uma assima da outra sem se sobreporem.

==codigo:
@
lado :: Float
lado = 60.0 
@
-}

lado :: Float 
lado = 60.0 





jogo1= Jogo (Jogador (5,8)) (Mapa 12 [(Estrada (1),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum])])