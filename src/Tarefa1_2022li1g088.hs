{- |
Module      : Tarefa1_2022li1g088
Description : Validação de um mapa
Copyright   : Paulo Alexandre Neves Moreira  <a64459 @alunos.uminho.pt>
              Silvério Mário Samuel <a101536@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g088 where

import LI12223
import Data.List (groupBy)
import Control.Arrow (Arrow(first))

mapaValido :: Mapa -> Bool
mapaValido mapa@(Mapa _ (((_, listadeobs):xs))) 
  | vervazios listadeobs && vernrobstaculos mapa && tipodeobs mapa && riospostos mapa && obsemlinha mapa && terrenoscontiguos mapa = True
  | otherwise = False


{-|Funcao que verifica que tem algum espaço com Nenhum obstaculo-}
vervazios :: [Obstaculo] -> Bool
vervazios [] = False
vervazios (x:xs) 
  | x== Nenhum = True
  |otherwise = vervazios xs
{-|funcao que valida que a largura é do tamanho da lista de obstaculos-}
vernrobstaculosfailed :: Mapa -> Bool
vernrobstaculosfailed (Mapa l ((_ , k):xs)) = l == length k


vernrobstaculos :: Mapa -> Bool
vernrobstaculos (Mapa l []) = True
vernrobstaculos (Mapa l ((_ , k):xs)) 
  | l == length k = vernrobstaculos (Mapa l (xs))
  | otherwise = False
{-|funcao que verifica se o Terreno tem algum Obstaculo nao permitido-}
tipodeobs :: Mapa -> Bool
tipodeobs (Mapa larg ([(terr, [])])) = True
tipodeobs (Mapa larg ([])) = True
tipodeobs (Mapa larg (((Relva, (x:xs)):ys)))
  | x == Arvore || x== Nenhum = tipodeobs (Mapa larg ((ys)))
  | otherwise = False
tipodeobs (Mapa larg (((Rio vel, (x:xs)):ys)))
  | x == Tronco || x== Nenhum = tipodeobs (Mapa larg ((ys)))
  | otherwise = False
tipodeobs (Mapa larg (((Estrada vel, (x:xs)):ys)))
  | x == Carro || x== Nenhum = tipodeobs (Mapa larg ((ys)))
  | otherwise = tipodeobs (Mapa larg ([(Estrada vel, (xs))]))

{-|funcao que valida que rios contiguos tem velocidade oposta-}

riospostos :: Mapa -> Bool
riospostos (Mapa larg ([])) = True
riospostos (Mapa larg (((Rio vel1, obst):(Rio vel2, obs):xs)))
  | vel1 * vel2 >= 0 = False
  | otherwise = riospostos (Mapa larg ((xs)))
riospostos _ = True
{-|funcao que valida o comprimento dos obstaculos(troncos) -}
troncoline :: Int -> (Terreno,[Obstaculo]) -> Bool
troncoline _ (terr, []) = True
troncoline 5 (terr, (x:xs)) = False
troncoline k (terr, (x:xs))
  | x== Tronco = troncoline (k + 1) (terr, (xs)) 
  | otherwise = troncoline (0) (terr, (xs))

{-|funcao que valida o comprimento dos obstaculos(carros) -}
carroline :: Int -> (Terreno,[Obstaculo]) -> Bool
carroline _ (terr, []) = True
carroline 3 (terr, (x:xs)) = False
carroline k (terr, (x:xs))
  | x== Carro = carroline (k + 1) (terr, (xs)) 
  | otherwise = carroline (0) (terr, (xs))


{-|juncao da carroline e troncoline-}
obsemlinha :: Mapa -> Bool
obsemlinha (Mapa l ([])) = True
obsemlinha (Mapa l (((terr, obs):xs))) 
 | carroline 0 (terr, obs) == False || troncoline 0 (terr, obs) == False = False
 | otherwise = obsemlinha (Mapa l ((xs))) 


{-obstaculoscontiguos :: Mapa -> Bool
obstaculoscontiguos (Mapa l ( [])) = True
obstaculoscontiguos mapa@(Mapa l ((x:xs)))
  | inicio (head(snd (head (head (agrupaOBSTACULOS mapatest))))) == "Tro" && length (snd (head(head (agrupaOBSTACULOS mapa)))) > 5 = False
  |inicio (head(snd (head (head (agrupaOBSTACULOS mapatest))))) == "Car" && length (snd (head(head (agrupaOBSTACULOS mapa)))) > 3 = False
  |otherwise = obstaculoscontiguos (Mapa l ((xs)))
agrupaOBSTACULOS :: Mapa -> [[(Terreno, [Obstaculo])]]
agrupaOBSTACULOS mapa@(Mapa _ (((terr, (a:b)):xs))) = groupBy (\x y -> (elem (take 3(show x)) [take 3 (show  y)]))  (Mapa l (((terr, (a:b)):xs)))-}

agrupa :: Eq a => [a] -> [[a]]
agrupa [] = []
agrupa [x] = [[x]]
agrupa (x:xs) 
  | elem x (head a) = (x: (head a)) : tail a
  | otherwise = [x] : a
    where a = agrupa xs

{-|funcao  que valida se na ha demasiados terrenos do mesmo tipo-}
terrenosseguidos :: Int -> Mapa -> Bool
terrenosseguidos _ (Mapa _ ([])) = True
terrenosseguidos 4 (Mapa l (((Rio _, _):xs))) = False
terrenosseguidos k (Mapa l (((Rio _, _):(xa:xs)))) =
     case xa of 
      (Rio _, _) -> terrenosseguidos (k+1) (Mapa l ((xa:xs)))
      _ -> terrenosseguidos 0 (Mapa l ((xa:xs)))
terrenosseguidos 5 (Mapa l (((Estrada _, _):xs))) = False
terrenosseguidos k (Mapa l (((Estrada _, _):xa:xs))) =
     case xa of 
      (Estrada _, _) -> terrenosseguidos (k+1) (Mapa l ((xa:xs)))
      _ -> terrenosseguidos 0 (Mapa l ((xa:xs)))
terrenosseguidos 5 (Mapa l (((Relva, _):xs))) = False
terrenosseguidos k (Mapa l (((Relva, _):xa:xs))) =
     case xa of 
      (Relva, _) -> terrenosseguidos (k+1) (Mapa l ((xa:xs)))
      _ -> terrenosseguidos 0 (Mapa l ((xa:xs)))

{-|Funcao que valida se ha 4 ou 5 terrenos xcontiguos dependendo do tipo de terreno-}

terrenoscontiguos :: Mapa -> Bool
terrenoscontiguos (Mapa _ (([]))) = True
terrenoscontiguos mapa@(Mapa l (((x):xs)))
  | inicio (fst(head (head (agrupaterrenos mapa)))) == "Est" && length (head (agrupaterrenos mapa)) >5 || inicio ( (head (agrupaterrenos mapa))) == "Rel" && length (head (agrupaterrenos mapa))  > 5 = False
  | inicio (fst(head a)) == "Rio" && length (head (agrupaterrenos mapa)) > 4 = False
  | otherwise = terrenoscontiguos (Mapa l (((xs))))
      where (a:b) = agrupaterrenos mapa
inicio :: Show a => a -> [Char]
inicio x =(take 3(show x))
agrupaterrenos :: Mapa -> [[(Terreno, [Obstaculo])]]
agrupaterrenos mapa@(Mapa _ (((terr, obst):xs))) = groupBy (\x y -> (elem (take 3(show x)) [take 3 (show  y)]))  ((terr, obst) : xs)

mapatest = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])
mapatest2 = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])