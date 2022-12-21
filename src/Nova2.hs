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
estadoInicial imagens = (PaginaJogar, jogo2, imagens)
--(PaginaJogar, (Jogo(Jogador (a,c))(Mapa 4 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum])]))

--jogo1 = Jogo (Jogador (2,2)) (Mapa 4 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum])])

jogo2= Jogo (Jogador (4,1)) (Mapa 9 [(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Nenhum]),(Rio 4,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Estrada (-2),[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro]),(Estrada 1,[Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum])]) 

desenhaMundo :: Mundo -> Picture
{-
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
-}
--PaginaJogar 
desenhaMundo (PaginaJogar, Jogo jogador (Mapa l ((te,obs):xs)), imagens) = Translate (-605) (-341) $ scale 2.65 1.45 $ Pictures world28 
 where 
     world28 = desenhaTerrenos {--++ [desenhajogador]--}
     desenhaTerrenos = criarMapa p o (getLargura (PaginaJogar, Jogo jogador (Mapa l ((te,obs):xs)), imagens)) (getTerreno (PaginaJogar, Jogo jogador (Mapa l ((te,obs):xs)), imagens)) imagens
     {--desenhajogador = criarJogador (getJogador (PaginaJogar, jogo, imagens)) imagens--} 

{-| Extrair o Mapa-}
getLargura :: Mundo -> Int 
getLargura (_, Jogo j (Mapa l ((te,obs):xs)), _) = l

getTerreno :: Mundo -> Terreno
getTerreno (_, Jogo j (Mapa l ((te,obs):xs)), _) = te

{-| Extrair o Jogador-}
--getJogador :: Mundo -> Jogador
--getJogador (_, jogo j m, _) = j

{-| Valor do x onde o(Relva,[Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]) Mapa vai comecar-}
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

desenhaLinha :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture]
desenhaLinha x y 0 te imagens = []
desenhaLinha x y la te imagens = terreno : linha 
                            where terreno = desenhaTer x y te imagens
                                  linha = desenhaLinha (x + lado) y (la-1) te imagens
desenhaLinha _ _ _ _ _ = []


-- desenhaLinhaobs :: Float -> Float -> Mapa -> Imagens -> [Picture]
-- desenhaLinhaobs x y (Mapa 0 ((te,obs):xs)) imagens = []
-- desenhaLinhaobs x y (Mapa la ((te,obs):xs)) imagens = terreno : linha 
--                             where obs = desenhaTer x y te imagens
--                                   linha = desenhaLinha (x + lado) y (Mapa (la-1) ((te,obs):xs)) imagens
-- desenhaLinhaobs _ _ _ _ = []


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

rio28 :: Picture 
rio28 = Color blue $ rectangleSolid lado lado  
relva28 :: Picture 
relva28 = Color green $ rectangleSolid lado lado  
estrada28 :: Picture 
estrada28 = Color black $ rectangleSolid lado lado 
{-| Funcao criarMapa 

Esta Funcao cria o Mapa usando o desenhalinha como auxiliar -}

criarMapa :: Float -> Float -> Int -> Terreno -> Imagens -> [Picture] 
criarMapa x y l te imagens = line ++ linhaseguinte
                             where line = desenhaLinha x y l te imagens 
                                   linhaseguinte = criarMapa x (y + lado) l te imagens 
criarMapa _ _ _ _ _ = []

{-| Criar Jogador

AINDA TENHO DUVIDAS NESTA FUNCAO-}
-- criarJogador :: Jogador -> Imagens -> Picture
-- criarJogador (Jogador (x,y)) imagens = Translate playerx playery (imagens !! 0)
--       where
--             playerx = (round x) * 60
--             playery = (round y) * 60

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
-- event (EventKey (SpecialKey KeyUp) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Cima-}jogo, imagens)
-- event (EventKey (SpecialKey KeyDown) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Baixo-}jogo, imagens)
-- event (EventKey (SpecialKey KeyLeft) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Esquerda-}jogo, imagens)
-- event (EventKey (SpecialKey KeyRight) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaJogar d, {-Funcao que move o Jogador para Direita-}jogo, imagens)
-- event (EventKey (SpecialKey KeySpace) Down _ _) (PaginaJogar d, jogo, imagens) = (PaginaPausa Continuar_1 d, jogo, imagens)
-- -- Caso qualquer outra coisa 
event _ s = s

{-| Funcao reage tempo 

Esta funcao com a estendeMapa como auxiliar, retira a ultima linha do mapa, e gera um mapa com uma nova linha na frente preservando assim o mesmo tamanho -}
reageTempo :: Float -> Mundo -> Mundo 
reageTempo _ s = s
--deslizaJogo a (Jogo (Jogador(x, y)) (Mapa l ((te,obs):xs))) = (Jogo (Jogador(x, y - l)) (Mapa l (init ((te,obs):xs))) (round a))

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
         let imagens = [galinha,scale 0.041 0.041 $ rio, relva28, estrada28, banner]
                  
         play window cor fr (estadoInicial imagens) desenhaMundo event reageTempo
