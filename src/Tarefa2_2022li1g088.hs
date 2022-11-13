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

Funcao que pega num Mapa com um conjunto de linhas e adiciona mais uma linha aleatoria
 -}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ((te,obs):xs)) a = Mapa l ((te2,obs2):(te,obs):xs)
                                 where te2 = ter29 a (te,obs) (ter28 a (Mapa l ((te,obs):xs))) 
                                       obs2 = obs28 l (te2, []) a
                           
 
{-| Funcao ter28

Funcao auxiliar da funcao estendeMapa, esta funcao pega num Mapa e devolve um possivel Terreno aleatorio 
-}                                  
ter28:: Int -> Mapa -> Terreno
ter28 a te' | aleatoriofinal a == 1 = head (proximosTerrenosValidos te')  
            | aleatoriofinal a == 2 = last (proximosTerrenosValidos te') 
            | otherwise = head (tail (proximosTerrenosValidos te'))   


{-| Funcao ter29

Esta funcao auxiliar da estedeMapa agrega um valor aleatorio entre (-4) a 4 para velocidade do Terreno aleatorio obtido na funcao ter28, ao mesmo tempo verefica o Terreno anterior e em casos especiais como o de gerar um Rio apos um Rio, ele se certifica de nao cria dois Rios seguidos a irem para a mesma direccao. Criando sempre Rios opostos
-}
ter29 :: Int -> (Terreno,[Obstaculo]) -> Terreno -> Terreno 
ter29 a (Rio vel1,obs) (Rio vel) | vel1 > 0 = Rio (aleatorio4'final a) 
                                 | otherwise = Rio (aleatorio4''final a)      
ter29 a (te,obs) (Rio vel) = Rio (aleatorio4final a)   
ter29 a (te,obs) (Estrada vel) = Estrada (aleatorio4final a)
ter29 a (te,obs) Relva = Relva     
         

{-|Funcao obs28

Esta funcao auxiliar da estendeMapa e responsavel por gerar aleatoriamente uma lista de Obstaculos possiveis, dado um determinado Terreno gerado.
-}
obs28 :: Int -> (Terreno,[Obstaculo]) -> Int -> [Obstaculo]
obs28 l (te2, b) a | l == (length b) = b
                   | otherwise = let obst = proximosObstaculosValidos l (te2, b)
                                     obs3 = obs3' obst a 
                                  in obs28 l (te2, b ++ [obs3] ) a 
{-|Funcao obs3'

Esta Funcao auxiliar da obs28 e respnsavel por selecionar aleatoriamente um Obstaculo para concatunar na obs28 para gerar a lista de obstaculos, para a nova linha gerada
-}
obs3' :: [Obstaculo] -> Int -> Obstaculo
obs3' b a | aleatoriofinal' a == 1 = head b
          | aleatoriofinal' a == 2 = last b
            

{-| Funcao Random

Funcoes que criam um valor aleatorio para ser usado pelas funcoes auxiliares do projecto
-}
listarandom :: Int -> Int -> [Int]
listarandom seed len = take len (randoms (mkStdGen seed))

{-| Funcao auxiliar que gera aleatoriamente valores entre 1 a 3-}
aleatoriofinal :: Int -> Int
aleatoriofinal k= 1+ abs ((head(listarandom (k) (1) )) `mod` (3))
{-| Funcao auxiliar que gera aleatoriamente valores entre 1 a 2-}
aleatoriofinal' :: Int -> Int
aleatoriofinal' k= 1+ abs ((head(listarandom (k) (1) )) `mod` (2))
{-| Funcao auxiliar para gerar numeros aleatorios entre 1 a 8 -}
so4 :: Int -> Int
so4 k = 1+ abs ((head(listarandom (k) (1) )) `mod` (8)) 
{-|Funcao auxiliar para gerar numeros aleatorios entre (-4) a 4 -}
aleatorio4final :: Int -> Int
aleatorio4final k 
  | so4 k >= 5 =  1 + (mod  (so4 k)  5)
  | otherwise =  -so4 k
{-|Funcao auxiliar para gerar numeros aleatorios entre (-4) a (-1) -}
aleatorio4'final :: Int -> Int
aleatorio4'final k 
  | so4 k >= 5 =  -(1 + (mod  (so4 k)  5))
  | otherwise  =  -so4 k
{-| Funcao auxiliar para gerar aleatoriamente numeros entre 1 a 4 -}
aleatorio4''final :: Int -> Int
aleatorio4''final k 
  | so4 k >= 5 =  1 + (mod  (so4 k)  5)
  | otherwise = so4 k

{-| proximosTerrenosValidos

Funcao que pega num Mapa e produz uma lista com os possiveis proximos terrenos validos para o Mapa e conforme as regras seguidas na ficha da T arefa1
-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva] 
proximosTerrenosValidos mapa@(Mapa l ((te,obs):xs))
                                              | tipodeobs mapa && terrenoscontiguos mapa && isrioFIM mapa = [Estrada 0, Relva]
                                              | tipodeobs mapa && terrenoscontiguos mapa && isestradaFIM mapa = [Rio 0, Relva]
                                              | tipodeobs mapa && terrenoscontiguos mapa && isrelvaFIM mapa = [Estrada 0, Rio 0]
                                              | not (tipodeobs mapa) || not (terrenoscontiguos mapa ) = []
                                              | otherwise = [Rio 0, Estrada 0, Relva]

{-| isrioFIM 

Funcao auxiliar que verifica se temos o numero limite de Rios para a funcao dos proximosTerrenosValidos -}
isrioFIM :: Mapa -> Bool
isrioFIM mapa = isRio2 1 mapa
{-| Auxiliar para isrioFIM-}
isRio2 :: Int -> Mapa -> Bool
isRio2 _ (Mapa _ []) = False
isRio2 4 (Mapa _ ((te,obs):xs)) = True 
isRio2 n (Mapa l ((te,obs):xs)) | inicionovo te == "Rio" = isRio2 (n+1)  (Mapa l (xs))
                                | otherwise = False
{-| isEstradaFIM 

Funcao auxiliar que verifica se temos o numero limite de Estradas para a funcao dos proximosTerrenosValidos -}
isestradaFIM :: Mapa -> Bool
isestradaFIM mapa = isEstrada2 1 mapa
{-Auxiliar para isEstradas-}
isEstrada2 :: Int -> Mapa -> Bool
isEstrada2 _ (Mapa _ []) = False
isEstrada2 5 (Mapa _ ((te,obs): xs)) = True
isEstrada2 n (Mapa l ((te,obs): xs)) | inicionovo te == "Est" = isEstrada2 (n+1) (Mapa l (xs))
                                     | otherwise = False 
{-| isrelvaFIM

Funcao auxiliar que verifica se temos o numero limite de Relvas para a funcao dos proximosTerrenosValidos -}                                    
isrelvaFIM :: Mapa -> Bool
isrelvaFIM mapa = isRelva2 1 mapa
{-| Auxiliar para isrelvaFIM-}
isRelva2 :: Int -> Mapa -> Bool
isRelva2 _ (Mapa _ []) = False 
isRelva2 5 (Mapa _ ((te,obs):xs)) = True
isRelva2 n (Mapa l ((te,obs):xs)) | inicionovo te == "Rel" = isRelva2 (n+1) (Mapa l (xs))
                                  | otherwise = False



{-| ProximosObstculos

Esta e uma das principais auxiliares da estendeMapa, que e uma funcao que verifica quais os possiveis proximos obstaculos validos.Ela pega num par de Terrenos e Obstaculos e procura exatamente o tipo de Obstaculo valido 
 -}
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

{-| Funcao auxiliar que verifica se os obstaculos correspondem ao tipo de Terreno.-}
tipobs :: (Terreno,[Obstaculo]) -> Bool
tipobs (_, []) = True
tipobs (te, (x:xs))
  | inicionovo te == "Rel" && x == Arvore || x == Nenhum = tipobs (te, xs) 
  | inicionovo te == "Rio" && x == Tronco || x == Nenhum = tipobs (te, xs)
  | inicionovo te == "Est" && x == Carro  || x == Nenhum = tipobs (te, xs)
  | otherwise = False


mapatest22 = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])

