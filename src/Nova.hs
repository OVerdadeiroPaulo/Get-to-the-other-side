module Main where

import LI12223
import Tarefa1_2022li1g088
import Tarefa2_2022li1g088
import Tarefa3_2022li1g088
import Tarefa4_2022li1g088
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

--NAO ESQUECER DE ADICIONAR DIFICULDADE AO DATA : JOGO
-- data Jogo =
--   Jogo
--     Jogador -- ^ o personagem do jogo
--     Mapa -- ^ o mapa em que se está a jogar
--   deriving (Show, Read, Eq)

-- -- | O Jogador define o personagem controlado no 'Jogo'.
-- newtype Jogador =
--   Jogador Coordenadas
--   deriving (Show, Read, Eq)

type Mundo = (Paginas, Jogo, Imagens)

type Imagens = [Picture]

{-| Menu que aparece quando entras no jogo -}
data MenuPrincipal = Jogar -- ^ Opcao para ir directamente ao jogo no nivel Facil ja predifinido 
                   | Dificuldades_1 -- ^ Opcao para ir ao Menu das dificuldades de jogo
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
             | PaginaPerdeuJogo MenuMorte Dificuldade -- ^ A pagina perdeu jogo mostranos opcoes do menu morte, tendo em conta a dicifuldade em que o jogador perdeu, para assim caso o jogador deseje reniciar ele possa reniciar no mesmo nivel
             | PaginaPausa Pausa Dificuldade -- ^ A pagina pausa mostranos as opcoes do menu pausa, e a dificuldade do jogo 
             | PaginaDificuldade Dificuldade Bool Dificuldade -- ^ A pagina dificuldades mostra opcoes de dificudade, tendo em conta se o menu anterior e um menu pausa ou principal
             | PaginaInstrucoes Bool Dificuldade -- ^ A pagina instrucoes mostra as instrucoes do jogo tendo em conta se o menu anterior era o menu pausa ou o menu principal 
             | PaginaMenuPausa MenuPausa Dificuldade -- ^ A pagina menu pausa mostranos as opcoes do menu pausa e a dificuldade em que o jogador esta 
             | PaginaJogar Dificuldade -- ^ A pagina jogar mostra que o jogador esta a jogar e a dificuldade em que joga


estadoInicial :: Mundo 
estadoInicial = (PaginaPrincipal Jogar, jogar, imagens)
-- estadoinicial = (PaginaJogar Facil, (Jogo(Jogador (a,c))(Mapa 4 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum])]))
         
desenhaMundo :: Mundo -> Pinture
--PaginaPrincipal 
desenhaMundo (PaginaPrincipal Jogar, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
desenhaMundo (PaginaPrincipal Dificuldades_1, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
desenhaMundo (PaginaPrincipal Instrucoes_1, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
desenhaMundo (PaginaPrincipal Sair_1, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
--PaginaPerdeuJogo
desenhaMundo (PaginaPerdeuJogo MudarDificuldade d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
desenhaMundo (PaginaPerdeuJogo Reniciar d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
desenhaMundo (PaginaPerdeuJogo Menu_3 d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
--PaginaPausa
desenhaMundo (Pausa Continuar_1 d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
desenhaMundo (Pausa Menu_2 d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
--PaginaDificuldade
desenhaMundo (PaginaDificuldade Facil b d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
desenhaMundo (PaginaDificuldade Media b d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
desenhaMundo (PaginaDificuldade Dificil b d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
desenhaMundo (PaginaDificuldade Menu1 b d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
--paginaInstrucoes 
desenhaMundo (PaginaInstrucoes b d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
--PaginaMenuPausa 
desenhaMundo (PaginaMenuPausa Continuar_2 d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)] 
desenhaMundo (PaginaMenuPausa NovoJogo d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
desenhaMundo (PaginaMenuPausa Dificuldades_2 d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
desenhaMundo (PaginaMenuPausa Instrucoes_2 d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
desenhaMundo (PaginaMenuPausa Sair_2 d, jogo, imagens) = Picture [Scale 0.0 0.0 (imagens !! 00)]
--PaginaJogar 
desenhaMundo (PaginaJogar d, jogo, imagens)
 | d == Facil = scale 0.0 0.0 $ Pictures world28 
 | d == Medio = scale 0.0 0.0 $ Pictures world28 
 | d == Facil = scale 0.0 0.0 $ Pictures world28 
 where 
     world28 = desenhamapa ++ [desenhajogador]
     desenhamapa = criarMapa p o (getMapa (PaginaJogar d, jogo, imagens)) imagens
     desenhajogador = criarJogador (getJogador (PaginaJogar d, jogo, imagens)) imagens 

{-| Extrair o Mapa-}
getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _) = m 

{-| Extrair o Jogador-}
getJogador :: Mundo -> Jogador
getJogador (_, jogo j m, _) = j

{-| Valor do x onde o Mapa vai comecar-}
p :: Float 
a = 0.0

{-| Valor do y onde o Mapa vai comecar-}
o :: Float 
c = 0.0

{-| Valor do lado da imagem, usado para contruir as figuras seguintes uma apos a outra e usado para controir as linhas uma assima da outra se se sobreporem-}
l :: Float 
l = 64.0 

{-| Funcao desenhaLinha

Funcao auxiliar que desenha uma linha do mapa -}

desenhaLinha :: Float -> Float -> Mapa -> Imagens -> [Picture]
desenhaLinha x y ((Mapa 0 ((te,obs):xs)) imagens = [] 
desenhaLinha x y ((Mapa la ((te,obs):xs)) imagens = terreno : linha 
                            where terreno = desenhaTer x y te imagens
                                  linha = desenhaLinha (x+l) y ((Mapa (la-1) ((te,obs):xs))
desenhaLinha _ _ _ _ = []

-- desenhaLinha :: Float -> Float -> Int -> (Terreno,[Obstaculo]) -> Imagens -> [Picture]
-- desenhaLinha x y 0 (terreno,z) imagens = [] 
-- desenhaLinha x y 0 (terreno,z) imagens = terreno : linha 
--                             where terreno = desenhaTer x y terreno imagens
--                                   linha = desenhaLinha (x+l) y ((Mapa (la-1) ((te,obs):xs))
-- desenhaLinha _ _ _ _ = []

{-| Funcao desenhaTer 

Com o valor x e y esta funcao cria uma picture com a imagem Terreno correspondente -}

desenhaTer :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer x y terreno imagens = Translate x y image 
                    where image = render terreno imagens 

{-| Funcao Render

Esta funcao junta cada terreno a uma imagem ja definida -}

render :: Terreno -> Imagens -> Picture 
render terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 2)
 | inicionovo terreno == "Rio" = (imagens !! 1)
 | inicionovo terreno == "Est" = (imagens !! 3)

{-| Funcao criarMapa 

Esta Funcao cria o Mapa usando o desenhalinha como auxiliar -}

criarMapa :: Float -> Float -> Mapa -> Imagens -> [Picture] 
criarMapa x y (Mapa l ((te,obs):xs)) imagens = line ++ linhaseguinte 
                                    where line = desenhaLinha x y (Mapa l ((te,obs):xs)) imagens 
                                          linhaseguinte = criarMapa x (y+l) (Mapa l (xs)) imagens 
criarMapa _ _ _ _ = []

{-| Criar Jogador

AINDA TENHO DUVIDAS NESTA FUNCAO-}
criarJogador :: Jogador -> Imagens -> Picture
criarJogador (Jogador (x,y)) imagens = Translate x y (image !! 0)

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

{-| Funcao deslizaJogo

Esta funcao com a estendeMapa como auxiliar, retira a ultima linha do mapa, e gera um mapa com uma nova linha na frente preservando assim o mesmo tamanho -}
deslizaJogo :: Float -> Jogo -> Jogo 
deslizaJogo a (Jogo (Jogador(x, y)) (Mapa l ((te,obs):xs))) = (Jogo (Jogador(x, y - l)) (Mapa l (init ((te,obs):xs))) (round a))

{-| Funcao Window 

Contem as definicoes do tamanho da tela, e neste caso vamos optar pelo Fullscreen que aproveita toda tela-}
window :: Display
window = FullScreen

{-| Funcao fr

Contem o numero de frames por segundo em que o nosso programa vai funcionar -}
fr :: Int
fr = 50


cor :: Color
cor = blue 

main :: IO ()
main = do 
         galinha <- loadBMP "Chicken_JE2_BE2.bmp"
         rio <- loadBMP "water-surface-texture-1928713.bmp"
         relva <- loadBMP "textura-da-grama-verde-textura-do-relvado-96665200.bmp"
         estrada <- loadBMP "textura-da-estrada-com-linhas-10054832(1).bmp"
         banner <- loadBMP "Banner_Video_Cover.bmp"
         let imagens = [galinha, rio, relva, estrada, banner]
         
         play window cor fr estadoInicial desenhaMundo event deslizajogo



{-criarJogador1 (Jogador (x,y)) t imagens (Move Baixo) | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens !! 153)
                                                     | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens !! 154)
                                                     | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens !! 155)
                                                     | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens !! 156)
                                                     | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens !! 157)
                                                     | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 158)
                                                     | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 159)
                                                     | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 160)
                                                     | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 161)
                                                     | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 162)
                                                     | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 163)
                                                     | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 164)
                                                     | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 165)
                                                     | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 166)
                                                     | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 167)
                                                     | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 168)
                                                     | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 169)
                                                     | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 170)
                                                     | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 171)
                                                     | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 172)
                                                     | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 173)
                                                     | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 174)
                                                     | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 175)
                                                     | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 176)
                                                     | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 177)
                                                     | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 178)
                                                     | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens !! 179)
                                                     | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens !! 180)
                                                     | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens !! 181)
                                                     | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens !! 182)
                                                     | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens !! 183)
                                                     | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens !! 184)
                                                     | (mod (round (t*22222)) 40) < 37 = Translate (saltaX x) (saltaY y) (imagens !! 185)
                                                     | (mod (round (t*22222)) 40) < 38 = Translate (saltaX x) (saltaY y) (imagens !! 186)-}



{-     desenhaTerrenos = criarMapa p o (getLargura (PaginaJogar, Jogo jogador (Mapa l ((te,obs):xs)), imagens)) (getTerreno (PaginaJogar, Jogo jogador (Mapa l ((te,obs):xs)), imagens)) imagens
-}


{-Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_1of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_2of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_3of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_4of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_5of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_6of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_7of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_8of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_9of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_10of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_11of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_12of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_13of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_14of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_15of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_16of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_17of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_18of19, Translate 0.0 40.0 $ scale 0.08 0.16 $ galinha_1Parado_19of19-}

--criarJogador1 :: Jogador -> Float-> Imagens -> Direcao -> Picture
-- criarJogador1 (Jogador (x,y)) t imagens Cima |(mod (round (t*22222)) 40) < 1 = Translate (saltaX x) (saltaY y) (imagens !! 34)
--                                              | (mod (round (t*22222)) 40) < 2 = Translate (saltaX x) (saltaY y) (imagens !! 35)
--                                              | (mod (round (t*22222)) 40) < 3 = Translate (saltaX x) (saltaY y) (imagens !! 36)
--                                              | (mod (round (t*22222)) 40) < 4 = Translate (saltaX x) (saltaY y) (imagens !! 37)
--                                              | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens !! 38)
--                                              | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens !! 39)
--                                              | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens !! 40)
--                                              | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens !! 41)
--                                              | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens !! 42)
--                                              | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 43)
--                                              | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 44)
--                                              | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 45)
--                                              | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 46)
--                                              | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 47)
--                                              | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 48)
--                                              | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 49)
--                                              | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 50)
--                                              | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 51)
--                                              | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 52)
--                                              | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 53)
--                                              | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 54)
--                                              | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 55)
--                                              | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 56)
--                                              | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 57)
--                                              | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 58)
--                                              | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 59)
--                                              | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 60)
--                                              | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 61)
--                                              | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 62)
--                                              | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 63)
--                                              | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens !! 64)
--                                              | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens !! 65)
--                                              | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens !! 66)
--                                              | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens !! 67)
--                                              | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens !! 68)
--                                              | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens !! 69)
--                                              | (mod (round (t*22222)) 40) < 37 = Translate (saltaX x) (saltaY y) (imagens !! 70)
--                                              | (mod (round (t*22222)) 40) < 38 = Translate (saltaX x) (saltaY y) (imagens !! 71)
--                                              | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 72)
-- criarJogador1 (Jogador (x,y)) t imagens Baixo |(mod (round (t*22222)) 40) < 1 = Translate (saltaX x) (saltaY y) (imagens !! 34)
--                                               | (mod (round (t*22222)) 40) < 2 = Translate (saltaX x) (saltaY y) (imagens !! 35)
--                                               | (mod (round (t*22222)) 40) < 3 = Translate (saltaX x) (saltaY y) (imagens !! 36)
--                                               | (mod (round (t*22222)) 40) < 4 = Translate (saltaX x) (saltaY y) (imagens !! 37)
--                                               | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens !! 38)
--                                               | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens !! 39)
--                                               | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens !! 40)
--                                               | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens !! 41)
--                                               | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens !! 42)
--                                               | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 43)
--                                               | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 44)
--                                               | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 45)
--                                               | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 46)
--                                               | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 47)
--                                               | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 48)
--                                               | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 49)
--                                               | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 50)
--                                               | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 51)
--                                               | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 52)
--                                               | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 53)
--                                               | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 54)
--                                               | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 55)
--                                               | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 56)
--                                               | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 57)
--                                               | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 58)
--                                               | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 59)
--                                               | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 60)
--                                               | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 61)
--                                               | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 62)
--                                               | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 63)
--                                               | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens !! 64)
--                                               | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens !! 65)
--                                               | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens !! 66)
--                                               | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens !! 67)
--                                               | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens !! 68)
--                                               | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens !! 69)
--                                               | (mod (round (t*22222)) 40) < 37 = Translate (saltaX x) (saltaY y) (imagens !! 70)
--                                               | (mod (round (t*22222)) 40) < 38 = Translate (saltaX x) (saltaY y) (imagens !! 71)
--                                               | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 72) 
-- criarJogador1 (Jogador (x,y)) t imagens Esquerda |(mod (round (t*22222)) 40) < 1 = Translate (saltaX x) (saltaY y) (imagens !! 73)
--                                                  | (mod (round (t*22222)) 40) < 2 = Translate (saltaX x) (saltaY y) (imagens !! 74)
--                                                  | (mod (round (t*22222)) 40) < 3 = Translate (saltaX x) (saltaY y) (imagens !! 75)
--                                                  | (mod (round (t*22222)) 40) < 4 = Translate (saltaX x) (saltaY y) (imagens !! 76)
--                                                  | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens !! 77)
--                                                  | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens !! 76)
--                                                  | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens !! 78)
--                                                  | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens !! 79)
--                                                  | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens !! 80)
--                                                  | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 81)
--                                                  | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 82)
--                                                  | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 83)
--                                                  | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 84)
--                                                  | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 85)
--                                                  | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 86)
--                                                  | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 87)
--                                                  | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 88)
--                                                  | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 89)
--                                                  | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 90)
--                                                  | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 91)
--                                                  | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 92)
--                                                  | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 93)
--                                                  | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 94)
--                                                  | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 95)
--                                                  | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 96)
--                                                  | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 97)
--                                                  | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 98)
--                                                  | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 99)
--                                                  | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 100)
--                                                  | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 101)
--                                                  | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens !! 102)
--                                                  | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens !! 103)
--                                                  | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens !! 104)
--                                                  | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens !! 105)
--                                                  | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens !! 106)
--                                                  | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens !! 107)
--                                                  | (mod (round (t*22222)) 40) < 37 = Translate (saltaX x) (saltaY y) (imagens !! 108)
--                                                  | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 109)  
-- criarJogador1 (Jogador (x,y)) t imagens Direita | (mod (round (t*22222)) 40) < 1 = Translate (saltaX x) (saltaY y) (imagens !! 110)
--                                                 | (mod (round (t*22222)) 40) < 2 = Translate (saltaX x) (saltaY y) (imagens !! 111)
--                                                 | (mod (round (t*22222)) 40) < 3 = Translate (saltaX x) (saltaY y) (imagens !! 112)
--                                                 | (mod (round (t*22222)) 40) < 4 = Translate (saltaX x) (saltaY y) (imagens !! 113)
--                                                 | (mod (round (t*22222)) 40) < 5 = Translate (saltaX x) (saltaY y) (imagens !! 114)
--                                                 | (mod (round (t*22222)) 40) < 6 = Translate (saltaX x) (saltaY y) (imagens !! 115)
--                                                 | (mod (round (t*22222)) 40) < 7 = Translate (saltaX x) (saltaY y) (imagens !! 116)
--                                                 | (mod (round (t*22222)) 40) < 8 = Translate (saltaX x) (saltaY y) (imagens !! 117)
--                                                 | (mod (round (t*22222)) 40) < 9 = Translate (saltaX x) (saltaY y) (imagens !! 118)
--                                                 | (mod (round (t*22222)) 40) < 10 = Translate (saltaX x) (saltaY y) (imagens !! 119)
--                                                 | (mod (round (t*22222)) 40) < 11 = Translate (saltaX x) (saltaY y) (imagens !! 120)
--                                                 | (mod (round (t*22222)) 40) < 12 = Translate (saltaX x) (saltaY y) (imagens !! 121)
--                                                 | (mod (round (t*22222)) 40) < 13 = Translate (saltaX x) (saltaY y) (imagens !! 122)
--                                                 | (mod (round (t*22222)) 40) < 14 = Translate (saltaX x) (saltaY y) (imagens !! 123)
--                                                 | (mod (round (t*22222)) 40) < 15 = Translate (saltaX x) (saltaY y) (imagens !! 124)
--                                                 | (mod (round (t*22222)) 40) < 16 = Translate (saltaX x) (saltaY y) (imagens !! 125)
--                                                 | (mod (round (t*22222)) 40) < 17 = Translate (saltaX x) (saltaY y) (imagens !! 126)
--                                                 | (mod (round (t*22222)) 40) < 18 = Translate (saltaX x) (saltaY y) (imagens !! 127)
--                                                 | (mod (round (t*22222)) 40) < 19 = Translate (saltaX x) (saltaY y) (imagens !! 128)
--                                                 | (mod (round (t*22222)) 40) < 20 = Translate (saltaX x) (saltaY y) (imagens !! 129)
--                                                 | (mod (round (t*22222)) 40) < 21 = Translate (saltaX x) (saltaY y) (imagens !! 130)
--                                                 | (mod (round (t*22222)) 40) < 22 = Translate (saltaX x) (saltaY y) (imagens !! 131)
--                                                 | (mod (round (t*22222)) 40) < 23 = Translate (saltaX x) (saltaY y) (imagens !! 132)
--                                                 | (mod (round (t*22222)) 40) < 24 = Translate (saltaX x) (saltaY y) (imagens !! 133)
--                                                 | (mod (round (t*22222)) 40) < 25 = Translate (saltaX x) (saltaY y) (imagens !! 134)
--                                                 | (mod (round (t*22222)) 40) < 26 = Translate (saltaX x) (saltaY y) (imagens !! 135)
--                                                 | (mod (round (t*22222)) 40) < 27 = Translate (saltaX x) (saltaY y) (imagens !! 136)
--                                                 | (mod (round (t*22222)) 40) < 28 = Translate (saltaX x) (saltaY y) (imagens !! 137)
--                                                 | (mod (round (t*22222)) 40) < 29 = Translate (saltaX x) (saltaY y) (imagens !! 140)
--                                                 | (mod (round (t*22222)) 40) < 30 = Translate (saltaX x) (saltaY y) (imagens !! 141)
--                                                 | (mod (round (t*22222)) 40) < 31 = Translate (saltaX x) (saltaY y) (imagens !! 142)
--                                                 | (mod (round (t*22222)) 40) < 32 = Translate (saltaX x) (saltaY y) (imagens !! 143)
--                                                 | (mod (round (t*22222)) 40) < 33 = Translate (saltaX x) (saltaY y) (imagens !! 144)
--                                                 | (mod (round (t*22222)) 40) < 34 = Translate (saltaX x) (saltaY y) (imagens !! 145)
--                                                 | (mod (round (t*22222)) 40) < 35 = Translate (saltaX x) (saltaY y) (imagens !! 146)
--                                                 | (mod (round (t*22222)) 40) < 36 = Translate (saltaX x) (saltaY y) (imagens !! 147)
--                                                 | otherwise = Translate (saltaX x) (saltaY y) (imagens !! 148)              
