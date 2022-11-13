{- |
Module      : Tarefa2_2022li1g088
Description : Geração contínua de um mapa
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g088 where

import LI12223 
import Tarefa1_2022li1g088
import System.Random
import LI12223 (Mapa, Terreno)
import Tarefa1_2022li1g088 (inicionovo)
import Tarefa1_2022li1g088 (tipodeobs)
import Data.List (elemIndex)
import Data.String (String)


{-| Funcao estendeMapa

Funcao que pega num Mapa com um conjunto de linhas e adiciona mais uma linha aleatoria por cima da linha existente. Usando as suas funcoes auxiliares ela cria uma linha nova com um terreno aleatorio dos proximosTerrenospossiveis e faz uma lista de obstaculos com somente os proximosObstaculos possiveis dessa lista tendo em conta o Terreno-}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((te,obs):xs)) a = Mapa l ((te2,obs2):(te,obs):xs)
                                 where te2 = ter29 a (te,obs) (ter28 a (Mapa l ((te,obs):xs))) 
                                       obs2 = obs28 l (te2, []) a
                           
 
{-| Funcao ter28

Funcao auxiliar da funcao estendeMapa, esta funcao pega num Mapa e devolve um Terreno possivel aleatorio. Com a ajuda da funcao proximosTerrenos e da aleatoriofinal, a funcao ter28, pega no resultado da funcao aleatoriedadefinal de a e compara com 1, 2 e 3, caso seja 1 ele imprimi o primeiro elemento da lista dos proximosTerrenosvalidos, 2 ele imprime o ultimo elemento e 3 ele imprime o elemento do meio.   -}                                  
ter28:: Int -> Mapa -> Terreno
ter28 a te' | aleatoriofinal a == 1 = head (proximosTerrenosValidos te')  
            | aleatoriofinal a == 2 = last (proximosTerrenosValidos te') 
            | otherwise = head (tail (proximosTerrenosValidos te'))   


{-| Funcao ter29

Esta funcao auxiliar da estedeMapa agrega um valor aleatorio entre (-4) a 4 para velocidade do Terreno aleatorio obtido na funcao ter28, ao mesmo tempo verefica o Terreno anterior e em casos especiais como o de gerar um Rio apos um Rio, ele se certifica de nao criar dois Rios seguidos a irem para a mesma direccao, criando sempre Rios opostos. 
A funcao funciona lendo a linha anterior com o Terreno que esta a ser criada e de seguida com as suas funcoes ela agrega uma velocidade aleatoria no Terreno que esta aser criado-}
ter29 :: Int -> (Terreno,[Obstaculo]) -> Terreno -> Terreno 
ter29 a (Rio vel1,obs) (Rio vel) | vel1 > 0 = Rio (aleatorio4'final a) 
                                 | otherwise = Rio (aleatorio4''final a)      
ter29 a (te,obs) (Rio vel) = Rio (aleatorio4final a)   
ter29 a (te,obs) (Estrada vel) = Estrada (aleatorio4final a)
ter29 a (te,obs) Relva = Relva     
         

{-|Funcao obs28

Esta funcao auxiliar da estendeMapa e responsavel por gerar aleatoriamente uma lista de Obstaculos possiveis, dado um determinado Terreno gerado. Junto da proximosOsbstaculosValidos esta funcao pega numa linha com um determinado terreno e cria uma lista de possiveis Obstaculos para criar a nova lista de obstaculos, e com a obs3 ela seleciona um dos obstaculos validos aleatoriamente e depois a obs28 trata de adicionar esse obstaculo a sua lista de obstaculos e voltar a fazer o processo recursivamente ate prencher a lista com Obstaculos de largura correta.
-}
obs28 :: Int -> (Terreno,[Obstaculo]) -> Int -> [Obstaculo]
obs28 l (te2, b) a | l == (length b) = b
                   | otherwise = let obst = proximosObstaculosValidos l (te2, b)
                                     obs3 = obs3' obst a 
                                  in obs28 l (te2, b ++ [obs3] ) a 
{-|Funcao obs3'

Esta Funcao auxiliar da obs28 e responsavel por selecionar aleatoriamente um dos Obstaculo para concatunar na obs28 para gerar a lista de obstaculos, para a nova linha gerada. Ela pega numa lista de obstaculos possiveis e com base no resultado ela seleciona a head ou o last da lista de obstaculospossiveis-}
obs3' :: [Obstaculo] -> Int -> Obstaculo
obs3' b a | aleatoriofinal' a == 1 = head b
          | aleatoriofinal' a == 2 = last b
            

{-| Funcoes Random

Funcoes que criam um valor aleatorio para ser usado pelas funcoes auxiliares do projecto. Esta funcao cria uma lista infinita de aleatorios e tira dessa lista uma lista finita de numeros aleatorios, que de seguida poderao ser usados nas outras funcoes aleatorias para criar aleatoriedades mais especifica-}
listarandom :: Int -> Int -> [Int]
listarandom seed len = take len (randoms (mkStdGen seed))

-- | Funcao auxiliar que gera aleatoriamente valores entre 1 a 3
aleatoriofinal :: Int -> Int
aleatoriofinal k= 1+ abs ((head(listarandom (k) (1) )) `mod` (3))
-- | Funcao auxiliar que gera aleatoriamente valores entre 1 a 2
aleatoriofinal' :: Int -> Int
aleatoriofinal' k= 1+ abs ((head(listarandom (k) (1) )) `mod` (2))
-- | Funcao auxiliar para gerar numeros aleatorios entre 1 a 8 
so4 :: Int -> Int
so4 k = 1+ abs ((head(listarandom (k) (1) )) `mod` (8)) 
-- | Funcao auxiliar para gerar numeros aleatorios entre (-4) a 4 
aleatorio4final :: Int -> Int
aleatorio4final k 
  | so4 k >= 5 =  1 + (mod  (so4 k)  5)
  | otherwise =  -so4 k
-- | Funcao auxiliar para gerar numeros aleatorios entre (-4) a (-1) 
aleatorio4'final :: Int -> Int
aleatorio4'final k 
  | so4 k >= 5 =  -(1 + (mod  (so4 k)  5))
  | otherwise  =  -so4 k
-- | Funcao auxiliar para gerar aleatoriamente numeros entre 1 a 4 
aleatorio4''final :: Int -> Int
aleatorio4''final k 
  | so4 k >= 5 =  1 + (mod  (so4 k)  5)
  | otherwise = so4 k

{-| proximosTerrenosValidos

Funcao que pega num Mapa e produz uma lista com os possiveis proximos terrenos validos para o Mapa tendo em conta as regras seguidas na ficha da Tarefa1. Esta funcao pega num mapa e compara com cada uma das regras de composicao dos terrenos e depois devolve somente a lista dos possiveis terrenos a adicionar na lista-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva] 
proximosTerrenosValidos mapa@(Mapa l ((te,obs):xs))
                                              | tipodeobs mapa && terrenoscontiguos mapa && isrioFIM mapa = [Estrada 0, Relva]
                                              | tipodeobs mapa && terrenoscontiguos mapa && isestradaFIM mapa = [Rio 0, Relva]
                                              | tipodeobs mapa && terrenoscontiguos mapa && isrelvaFIM mapa = [Estrada 0, Rio 0]
                                              | not (tipodeobs mapa) || not (terrenoscontiguos mapa ) = []
                                              | otherwise = [Rio 0, Estrada 0, Relva]

{-| isrioFIM 

Funcao auxiliar que verifica se temos o numero limite de Rios para a funcao dos proximosTerrenosValidos. Esta funcao pega no Mapa dado e usando um contador ela verifica se os primeiros 4 elementos seguidos da funcao sao Rios sem interrupcao, representando assim a regra que diz que se houverem quatro rios seguidos, ja nao podemos adicionar mais Rios a frente -}
isrioFIM :: Mapa -> Bool
isrioFIM mapa = isRio2 1 mapa
{-| Contador auxiliar para isrioFIM-}
isRio2 :: Int -> Mapa -> Bool
isRio2 _ (Mapa _ []) = False
isRio2 4 (Mapa _ ((te,obs):xs)) = True 
isRio2 n (Mapa l ((te,obs):xs)) | inicionovo te == "Rio" = isRio2 (n+1)  (Mapa l (xs))
                                | otherwise = False
{-| isEstradaFIM 

Funcao auxiliar que verifica se temos o numero limite de Estradas para a funcao dos proximosTerrenosValidos. Esta funcao pega no Mapa dado e usando um contador ela verifica se os primeiros 5 elementos seguidos da funcao sao Estrada sem interrupcao, representando assim a regra que diz que se houverem cinco Estradas seguidas, ja nao podemos adicionar mais Estradas a frente  -}
isestradaFIM :: Mapa -> Bool
isestradaFIM mapa = isEstrada2 1 mapa
{-| Contador auxiliar para isestradasFIM-}
isEstrada2 :: Int -> Mapa -> Bool
isEstrada2 _ (Mapa _ []) = False
isEstrada2 5 (Mapa _ ((te,obs): xs)) = True
isEstrada2 n (Mapa l ((te,obs): xs)) | inicionovo te == "Est" = isEstrada2 (n+1) (Mapa l (xs))
                                     | otherwise = False 
{-| isrelvaFIM

Funcao auxiliar que verifica se temos o numero limite de Relvas para a funcao dos proximosTerrenosValidos.Esta funcao pega no Mapa dado e usando um contador ela verifica se os primeiros 5 elementos seguidos da funcao sao Relva sem interrupcao, representando assim a regra que diz que se ouverem cinco Relvas seguidas, ja nao podemos adicionar mais Relvas a frente-}                                    
isrelvaFIM :: Mapa -> Bool
isrelvaFIM mapa = isRelva2 1 mapa
{-| Contador auxiliar para isrelvaFIM-}
isRelva2 :: Int -> Mapa -> Bool
isRelva2 _ (Mapa _ []) = False 
isRelva2 5 (Mapa _ ((te,obs):xs)) = True
isRelva2 n (Mapa l ((te,obs):xs)) | inicionovo te == "Rel" = isRelva2 (n+1) (Mapa l (xs))
                                  | otherwise = False



{-| ProximosObstculos

Esta e uma das principais auxiliares da estendeMapa, que e uma funcao que verifica quais os possiveis proximos obstaculos validos. Ela pega num par de Terreno e Obstaculos e compara com todas as regras da Tarefa1 para composicao dos obstaculos, tal como nao ter uma lista de Obstaculos so com o mesmo elemento e nao dar uma lista de obstaculos a Um Terrno com Obstaculos invalidos-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos _ (Relva, []) = [Nenhum,Arvore]
proximosObstaculosValidos _ (Estrada vel, []) = [Nenhum,Carro]
proximosObstaculosValidos n (te, (x:xs)) | inicionovo te == "Rio" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]                        
                                         | inicionovo te == "Rio" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Tronco (x:xs)) = [Tronco]                     
                                         | inicionovo te == "Rio" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum,Tronco]                     
                                         | inicionovo te == "Rel" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]
                                         | inicionovo te == "Rel" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Arvore (x:xs)) = [Arvore] 
                                         | inicionovo te == "Rel" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum,Arvore]
                                         | inicionovo te == "Est" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]
                                         | inicionovo te == "Est" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Carro (x:xs)) = [Carro] 
                                         | inicionovo te == "Est" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum, Carro]
                                         | otherwise = []

{-| tipoobs

Funcao auxiliar que verifica se os obstaculos correspondem ao tipo de Terreno dado. Esta funcao pega numa linha e compara cada elememdo dos obstaculos dessa linha com os possiveis Obstaculos desse terreno.-}
tipobs :: (Terreno,[Obstaculo]) -> Bool
tipobs (_, []) = True
tipobs (te, (x:xs))
  | inicionovo te == "Rel" && x == Arvore || x == Nenhum = tipobs (te, xs) 
  | inicionovo te == "Rio" && x == Tronco || x == Nenhum = tipobs (te, xs)
  | inicionovo te == "Est" && x == Carro  || x == Nenhum = tipobs (te, xs)
  | otherwise = False


-- | Mapa que devolve Mapa 4 [(Rio 3,[Nenhum,Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum,Carro])]
estendeMapatest1 = Mapa 4 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum])] 
-- | Teste de estencao de Mapa com 5 relvas seguidas (estendeMapa)
estendeMapatest2 = Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada 2,[Nenhum,Carro]),(Estrada 3,[Carro,Nenhum])] 
-- | Teste de estencao de Mapa com 5 Estradas seguidas (estendeMapa)
estendeMapatest3 = Mapa 2 [(Estrada (-2),[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])] 
-- | Teste de estencao de Mapa com 4 Rios seguidos (estendeMapa)
estendeMapatest4 = Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Rio 3, [Tronco,Tronco,Nenhum])] 
-- | Teste random para estender um mapa comum (estendeMapa)
estendeMapatest5 = Mapa 3 [(Rio (-2), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Nenhum]),(Relva, [Arvore,Nenhum,Arvore])] 
-- | Teste para ver os possiveis Terrenos possiveis de uma lista vazio 
proximosTerrenosValidostest1 = Mapa 4 []
-- | Teste para ver os possiveis Terrenos validos de uma lista com 4 Rios seguidos  
proximosTerrenosValidostest2 = Mapa 3 [(Rio (-1), [Tronco,Nenhum,Tronco]),(Rio 2, [Tronco,Nenhum,Tronco]),(Rio (-2), [Tronco,Nenhum,Nenhum]),(Rio 4, [Tronco,Tronco,Nenhum])] 
-- | Teste para ver os possiveis Terrenos validos de uma lista com 5 Estradas seguidas   
proximosTerrenosValidostest3 = Mapa 2 [(Estrada (-2),[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Estrada (-2),[Nenhum,Carro]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Estrada 3,[Carro,Nenhum])] 
-- | Testes para ver os possiveis Terrenos validos de uma lista com 5 Relvas seguidas
proximosTerrenosValidostest4 = Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Nenhum]),(Relva,[Arvore,Nenhum,Arvore]),(Relva,[Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Arvore]),(Estrada 1,[Carro,Nenhum,Carro]),(Estrada 2,[Carro,Nenhum,Carro]),(Estrada 3,[Nenhum,Carro,Nenhum])] 
-- | Testes para ver os possiveis Terrenos validos de uma lista aleatoria
proximosTerrenosValidostest5 = Mapa 4 [(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 4,[Tronco,Nenhum,Tronco,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Arvore,Nenhum]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum,Nenhum])]
-- | Testes para ver os possiveis Terrenos validos de uma lista invalida 
proximosTerrenosValidostest6 = Mapa 3 [(Rio (-4), [Tronco,Nenhum,Tronco]),(Rio 3, [Tronco,Nenhum,Tronco]),(Rio (-1), [Tronco,Nenhum,Carro]),(Relva, [Arvore,Nenhum,Carro])]
-- | Testes para ver os possiveis obstaculos de uma lista random de Rios
proximosObstaculosValidos1 = (Rio 4,[Tronco,Nenhum,Tronco,Nenhum])
-- | Teste para ver os possiveis obstaculos de uma lista vazia 
proximosObstaculosValidos2 = (Estrada 3,[])
-- | Teste para ver os possiveis obstaculos de uma lista  sem nenhuns faltando somente 1 para completar a lista 
proximosObstaculosValidos3 = (Relva,[Arvore,Arvore,Arvore])
-- | Testes para ver os possiveis obstaculos numa lista so de nenhuns faltando 1 para completar a lista 
proximosObstaculosValidos4 = (Rio 4,[Nenhum,Nenhum]) 
