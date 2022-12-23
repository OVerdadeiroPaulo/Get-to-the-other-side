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
                   | Instrucoes_1 -- ^ Opcao para ver as instrucoes e objectivos do jogo 
                   | Sair_1 -- ^ Opcao para sair do jogo 
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
             | PaginaPerdeuJogo MenuMorte-- ^ A pagina perdeu jogo mostranos opcoes do menu morte
             | PaginaPausa Pausa -- ^ A pagina pausa mostranos as opcoes do menu pausa 
             | PaginaInstrucoes Bool-- ^ Apresenta as instrucoes 
             | PaginaMenuPausa Bool MenuPausa -- ^ A pagina menu pausa mostranos as opcoes do menu pausa  
             | PaginaJogar -- ^ A pagina jogar mostra que o jogador esta a jogar 


estadoInicial :: Imagens -> Mundo 
estadoInicial imagens = (PaginaJogar, jogo1, imagens)
--(PaginaJogar, (Jogo(Jogador (a,c))(Mapa 4 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum])]))
jogo1= Jogo (Jogador (,0)) (Mapa 9 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore])]) 

mapa1 = (Mapa 9 [(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum])])

desenhaMundo :: Mundo -> Picture
--PaginaPrincipal 
desenhaMundo (PaginaPrincipal Jogar, jogo, imagens) = Pictures [Scale 1.0 1.0 (imagens !! 4)] 
desenhaMundo (PaginaPrincipal Instrucoes_1, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)] 
desenhaMundo (PaginaPrincipal Sair_1, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)] 
--PaginaPerdeuJogo
desenhaMundo (PaginaPerdeuJogo Reniciar, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)]
desenhaMundo (PaginaPerdeuJogo Menu_3, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)]
--PaginaPausa
desenhaMundo (PaginaPausa Continuar_1, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)]
desenhaMundo (PaginaPausa Menu_2, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)]
--paginaInstrucoes 
desenhaMundo (PaginaInstrucoes b, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)] 
--PaginaMenuPausa 
desenhaMundo (PaginaMenuPausa b Continuar_2, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)] 
desenhaMundo (PaginaMenuPausa b NovoJogo, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa b Instrucoes_2, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)]
desenhaMundo (PaginaMenuPausa b Sair_2, jogo, imagens) = Pictures [Scale 2.0 2.0 (imagens !! 4)]
--PaginaJogar 
desenhaMundo (PaginaJogar, jogo, imagens) = Translate (-605) (341) $ scale 2.65 1.45 $ Pictures world28 
 where 
     world28 = desenhaTerrenos ++ desenhaObstaculos ++ [desenhajogador]
     desenhaTerrenos = criarTerreno p o (getLargura(getMapa (PaginaJogar, jogo, imagens))) (getTerreno(getMapa (PaginaJogar, jogo, imagens))) imagens
     desenhaObstaculos = criarObstaculos p o (getTerreno(getMapa (PaginaJogar, jogo, imagens))) imagens 
     desenhajogador = criarJogador (getJogador (PaginaJogar, jogo, imagens)) imagens


{-| Extrair o Mapa-}
getMapa :: Mundo -> Mapa 
getMapa (_, Jogo j m, _) = m 

getLargura :: Mapa -> Int 
getLargura (Mapa l ((te,obs):xs)) = l

getTerreno :: Mapa -> [(Terreno,[Obstaculo])] 
getTerreno (Mapa l ((te,obs):xs)) = ((te,obs):xs)


{-| Extrair o Jogador-}
getJogador :: Mundo -> Jogador
getJogador (_, Jogo j m, _) = j

{-| Valor do x onde o Mapa vai comecar-}
p :: Float 
p = 0.0

{-| Valor do y onde o Mapa vai comecar-}
o :: Float 
o = 0.0

{-| Valor do lado da imagem, usado para contruir as figuras seguintes uma apos a outra e usado para controir as linhas uma assima da outra se se sobreporem-}
lado :: Float 
lado = 60.0 

{-| Funcao desenhaLinha

Funcao auxiliar que desenha uma linha do mapa -}

desenhaLinhaTer :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinhaTer x y 0 te imagens = []
desenhaLinhaTer x y la te imagens = terreno : linha 
                            where terreno = desenhaTer x y te imagens
                                  linha = desenhaLinhaTer (x + lado) y (la-1) te imagens
desenhaLinhaTer _ _ _ _ _ = []


desenhaLinhaObs :: Float -> Float -> [Obstaculo] -> Imagens -> [Picture]
desenhaLinhaObs x y [] imagens = []
desenhaLinhaObs x y (z:zs) imagens = obstaculos : linha
                                 where obstaculos = desenhaObs x y z imagens 
                                       linha = desenhaLinhaObs (x + lado) y zs imagens
desenhaLinhaObs _ _ _ _ = []
{-| Funcao desenhaTer 

Com o valor x e y esta funcao cria uma picture com a imagem Terreno correspondente -}

desenhaTer :: Float -> Float -> Terreno -> Imagens -> Picture 
desenhaTer x y terreno imagens = Translate x y image 
                       where image = render terreno imagens 

desenhaObs :: Float -> Float -> Obstaculo -> Imagens -> Picture
desenhaObs x y obstaculo imagens = Translate x y image
                         where image = render2 obstaculo imagens 
{-| Funcao Render

Esta funcao junta cada terreno a uma imagem ja definida -}

render :: Terreno -> Imagens -> Picture 
render terreno imagens
 | inicionovo terreno == "Rel" = (imagens !! 2)
 | inicionovo terreno == "Rio" = (imagens !! 1)
 | inicionovo terreno == "Est" = (imagens !! 3)

render2 :: Obstaculo -> Imagens -> Picture 
render2 obstaculo imagens 
 | obstaculo == Nenhum = (imagens !! 8)
 | obstaculo == Tronco = (imagens !! 5)
 | obstaculo == Arvore = (imagens !! 6)
 | obstaculo == Carro = (imagens !! 7)

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

{-| Funcao criarMapa 

Esta Funcao cria o Mapa usando o desenhalinha como auxiliar -}

criarTerreno :: Float -> Float -> Int -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] 
criarTerreno x y la ((te,obs):xs) imagens = line ++ linhaseguinte 
                              where line = desenhaLinhaTer x y la te imagens 
                                    linhaseguinte = criarTerreno x (y - lado) la (xs) imagens 
criarTerreno _ _ _ _ _ = []

criarObstaculos :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture]
criarObstaculos x y ((z,w):zs) imagens = line ++ linhaseguinte
                                 where line = desenhaLinhaObs x y w imagens
                                       linhaseguinte = criarObstaculos x (y - lado) (zs) imagens 
criarObstaculos _ _ _ _ = [] 

{-| Criar Jogador -}

criarJogador :: Jogador -> Imagens -> Picture
criarJogador (Jogador (x,y)) imagens = Translate (saltaX x) (saltaY y) (imagens !! 0)
                             
                              
saltaX :: Int -> Float 
saltaX = (+p).(*lado).realToFrac
                              
saltaY :: Int -> Float 
saltaY = (+o).(*lado).realToFrac                           



event :: Event -> Mundo -> Mundo 
-- -- Pagina Principal 
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Jogar, jogo, imagens) = (PaginaPrincipal Sair_1, jogo, imagens) 
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens) = (PaginaPrincipal Jogar, jogo, imagens) 
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens) = (PaginaPrincipal Instrucoes_1, jogo, imagens) 
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Jogar, jogo, imagens) = (PaginaPrincipal Instrucoes_1, jogo, imagens) 
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens) = (PaginaPrincipal Sair_1, jogo, imagens) 
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens) = (PaginaPrincipal Jogar, jogo, imagens) 
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Jogar, Jogo j m, imagens) = (PaginaJogar Facil, Jogo j m, imagens) 
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Instrucoes_1, jogo, imagens) = (PaginaInstrucoes false Facil, jogo, imagens) 
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPrincipal Sair_1, jogo, imagens) = error "Jogo Terminou"
-- -- Pagina controlos  
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaInstrucoes b, jogo, imagens) | b == True = (PaginaMenuPausa Continuar_2, jogo, imagens)
--                                                                                  | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaInstrucoes b, jogo, imagens) | b == True = (PaginaMenuPausa Continuar_2, jogo, imagens)
--                                                                                    | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaInstrucoes b, jogo, imagens) | b == True = (PaginaMenuPausa Continuar_2, jogo, imagens)
--                                                                                     | otherwise = (PaginaPrincipal Instrucoes_1, jogo, imagens)
-- -- Pagina Pausa  
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Continuar_1, Jogo j m, imagens) = (PaginaPausa Menu_2, Jogo j m, imagens)
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPausa Menu_2, Jogo j m, imagens) = (PaginaPausa Continuar_1, Jogo j m, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Continuar_1, Jogo j m, imagens) = (PaginaPausa Menu_2, Jogo j m, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPausa Menu_2, Jogo j m, imagens) = (PaginaPausa Continuar_1, Jogo j m, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Continuar_1, jogo, imagens) = (PaginaJogar, jogo, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPausa Menu_2, jogo, imagens) = (PaginaMenuPausa Continuar_2, jogo, imagens)
-- -- Pagina MenuPausa 
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Continuar_2, jogo, imagens) = (PaginaMenuPausa Sair_2, jogo, imagens)
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa NovoJogo, jogo, imagens) = (PaginaMenuPausa Continuar_2, jogo, imagens)
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Instrucoes_2, jogo, imagens) = (PaginaMenuPausa NovoJogo, jogo, imagens)
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaMenuPausa Sair_2, jogo, imagens) = (PaginaMenuPausa Instrucoes_2, jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Continuar_2, jogo, imagens) = (PaginaMenuPausa NovoJogo, jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa NovoJogo, jogo, imagens) = (PaginaMenuPausa Instrucoes_2, jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Instrucoes_2, jogo, imagens) = (PaginaMenuPausa Sair_2, jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaMenuPausa Sair_2, jogo, imagens) = (PaginaMenuPausa Continuar_2, jogo, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Continuar_2, jogo, imagens) = (PaginaJogar, jogo, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa NovoJogo, jogo, imagens) = (PaginaJogar, Jogo j m, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Instrucoes_2, jogo, imagens) = (PaginaInstrucoes True, jogo, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaMenuPausa Sair_2, jogo, imagens) = error "Terminou Jogo"
-- -- Pagina Perdeu jogo
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Reniciar, jogo, imagens) = (PaginaPerdeuJogo Menu_3, jogo, imagens)
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaPerdeuJogo Menu_3, jogo, imagens) = (PaginaPerdeuJogo Reniciar, jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Reniciar, jogo, imagens) = (PaginaPerdeuJogo Menu_3, jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaPerdeuJogo Menu_3, jogo, imagens) = (PaginaPerdeuJogo Reniciar, jogo, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Reniciar, jogo, imagens) = (PaginaJogar, Jogo j m, imagens)
-- event (EventKey (SpecialKey KeyEnter) Down _ _) (PaginaPerdeuJogo Menu_3, jogo, imagens) = (PaginaPrincipal Jogar, jogo, imagens)
-- -- Pagina Jogar 
event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaJogar, jogo, imagens) = (PaginaJogar, animaJogo jogo (Move Baixo), imagens)
event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaJogar, jogo, imagens) = (PaginaJogar, animaJogo jogo (Move Cima), imagens)
event (EventKey (SpecialKey KeyLeft) Down _ _) (PaginaJogar, jogo, imagens) = (PaginaJogar, animaJogo jogo (Move Esquerda), imagens)
event (EventKey (SpecialKey KeyRight) Down _ _) (PaginaJogar, jogo, imagens) = (PaginaJogar, animaJogo jogo (Move Direita), imagens)
event (EventKey (SpecialKey KeySpace) Down _ _) (PaginaJogar, jogo, imagens) = (PaginaPausa Continuar_1, jogo, imagens)
event _ s = s

{-| Funcao deslizaJogo

Esta funcao com a estendeMapa como auxiliar, retira a ultima linha do mapa, e gera um mapa com uma nova linha na frente preservando assim o mesmo tamanho -}
-- deslizaJogo :: Int -> Jogo -> Jogo 
-- deslizaJogo a (Jogo (Jogador(x, y)) (Mapa l ((te,obs):xs))) | (mod a 5) == 0 =  (Jogo (Jogador(x, y - 1)) (estendeMapa (Mapa l (drop 1 ((te,obs):xs))) a))
--                                                             | otherwise = (Jogo (Jogador(x, y)) (Mapa l ((te,obs):xs)))
                                                             
--deslizaJogo :: Int -> Jogo -> Jogo 
--deslizaJogo a (Jogo (Jogador(x, y)) (Mapa l ((te,obs):xs))) =  (Jogo (Jogador(x, y + 1)) (estendeMapa (Mapa l (drop 1 ((te,obs):xs))) a))
                                                           

reageTempo :: Float -> Mundo -> Mundo 
--reageTempo z (PaginaJogar, jogo, imagens) = (PaginaJogar, deslizaJogo (round z) jogo, imagens) 
reageTempo _ z = z

{-| Funcao Window 

Contem as definicoes do tamanho da tela, e neste caso vamos optar pelo Fullscreen que aproveita toda tela-}
window :: Display
window = FullScreen

{-| Funcao fr

Contem o numero de frames por segundo em que o nosso programa vai funcionar -}
fr :: Int
fr = 1


cor :: Color
cor = cyan 

main :: IO ()
main = do 
         galinha <- loadBMP "Chicken_JE2_BE2.bmp"
         rio <- loadBMP "water-surface-texture-1928713.bmp"
         relva <- loadBMP "textura-da-grama-verde-textura-do-relvado-96665200.bmp"
         estrada <- loadBMP "textura-da-estrada-com-linhas-10054832(1).bmp"
         banner <- loadBMP "Banner_Video_Cover.bmp"
         let imagens = [galinha28,scale 0.041 0.041 $ rio, relva28, estrada28, banner, tronco28, arvore28, carro28, nenhum28]
                  
         play window cor fr (estadoInicial imagens) desenhaMundo event reageTempo
