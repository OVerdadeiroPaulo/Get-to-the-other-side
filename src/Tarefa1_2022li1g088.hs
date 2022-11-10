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
tipodeobsaux :: Mapa -> Bool
tipodeobsaux (Mapa larg (((_, []) :y))) = True
tipodeobsaux (Mapa larg (((Relva, (x:xs)):ys)))
  | x==Carro ||x==Tronco = False
  | otherwise = tipodeobsaux (Mapa larg (((Relva, (xs)):ys)))
tipodeobsaux (Mapa larg (((Rio vel, (x:xs)):ys)))
  | x==Carro ||x==Arvore = False
  |otherwise = tipodeobsaux (Mapa larg (((Rio vel, (xs)):ys)))
tipodeobsaux (Mapa larg (((Estrada vel, (x:xs)):ys)))
  | x==Tronco || x==Arvore = False
  |otherwise = tipodeobsaux (Mapa larg (((Estrada vel, (xs)):ys)))
 {-|versaO 2-}
tipodeaux :: Mapa -> Bool
tipodeaux (Mapa l []) = True
tipodeaux (Mapa l (((terr, []):ys))) = True
tipodeaux (Mapa l (((terr, [x]):ys)))
  | inicio terr == "Rel" && (x == Carro || x== Tronco) = False
  | inicio terr == "Rio" && (x == Carro || x== Arvore) = False
  | inicio terr == "Est" && (x == Tronco || x == Arvore) = False
  | otherwise = True
tipodeaux (Mapa l ([(terr, (x:xs))]))
  | inicio terr == "Rel" && ( x == Carro ||  x== Tronco) = False
  | inicio terr == "Est" && ( x == Arvore ||  x== Tronco) = False
  | inicio terr == "Rio" && ( x == Carro ||  x== Arvore) = False
  | otherwise = tipodeaux (Mapa l ([(terr, (xs))]))

tipodeaux (Mapa l (((terr, (x:xs)):ys)))
  | inicio terr == "Rel" && (x == Carro || x== Tronco) = False
  | inicio terr == "Rio" && (x == Carro || x== Arvore) = False
  | inicio terr == "Est" && (x == Tronco || x == Arvore) = False
  | otherwise = tipodeaux (Mapa l (((terr, (xs))):ys))

{-|funcao que valida se existe algum obstaculo invalido em varial inhas usando a tipodeobsaux-}

tipodeobs :: Mapa -> Bool
tipodeobs (Mapa larg ([]))= True
tipodeobs (Mapa larg (((terr, (xs)):ys))) 
  | tipodeaux (Mapa larg (((terr, (xs)):ys))) == False = False
  | otherwise = tipodeaux (Mapa larg ((ys)))

{-|funcao que valida que rios contiguos tem velocidade oposta-}

riospostos :: Mapa -> Bool
riospostos (Mapa larg ([])) = True
riospostos (Mapa larg (((Rio vel1, obst):(Rio vel2, obs):xs)))
  | vel1 * vel2 >= 0 = False
  | otherwise = riospostos (Mapa larg ((xs)))
riospostos _ = True
{-|funcao que valida o comprimento dos obstaculos(troncos) -}


veostroncos :: (Terreno, [Obstaculo]) -> Bool
veostroncos (a, []) = True
veostroncos (a,[h,t]) = True
veostroncos vari@(a,(h:t))
  | head x == Tronco && length x >= 5 = False
  | head x == Tronco &&  elem Tronco (last xs) && (length x) + (length (last (x:xs))) >= 3 = False
  | otherwise = veostroncos (a,(t))
      where (x:xs) = agrupaobs (h:t)


{-|auxiliar para veroscarrose verostroncos-}
agrupaobs :: Eq a => [a] -> [[a]]
agrupaobs [] = []
agrupaobs [x] = [[x]]
agrupaobs (x:xs) 
  | elem x (head a) = (x: (head a)) : tail a
  | otherwise = [x] : a
     where a = agrupaobs xs

{-|funcao que valida o comprimento dos obstaculos(carros) -}
veoscarros :: (Terreno, [Obstaculo]) -> Bool
veoscarros (a, []) = True
veoscarros (a,[h,t]) = True
veoscarros vari@(a,(h:t))
  | head x == Carro && length x >= 3 = False
  | head x == Carro &&  elem Carro (last xs) && (length x) + (length (last (x:xs))) >= 5 = False
  | otherwise = veoscarros (a,(t))
      where (x:xs) = agrupaobs (h:t)



{-|juncao da veoscarros e veostroncos-}
obsemlinha :: Mapa -> Bool
obsemlinha (Mapa l ([])) = True
obsemlinha (Mapa l (((terr, obs):xs))) 
 | veoscarros (terr, obs) == False || veostroncos (terr, obs) == False = False
 | otherwise = obsemlinha (Mapa l ((xs))) 


{-obstaculoscontiguos :: Mapa -> Bool
obstaculoscontiguos (Mapa l ( [])) = True
obstaculoscontiguos mapa@(Mapa l ((x:xs)))
  | inicio (head(snd (head (head (agrupaOBSTACULOS mapatest))))) == "Tro" && length (snd (head(head (agrupaOBSTACULOS mapa)))) > 5 = False
  |inicio (head(snd (head (head (agrupaOBSTACULOS mapatest))))) == "Car" && length (snd (head(head (agrupaOBSTACULOS mapa)))) > 3 = False
  |otherwise = obstaculoscontiguos (Mapa l ((xs)))
-}


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

{-|Funcao que valida se ha 4 ou 5 terrenos contiguos dependendo do tipo de terreno-}

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
mapatest2 = Mapa 9 ([(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])])
mapatestfailtipodeobs = Mapa 9 ([(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum,Carro,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])])
mapatestfaillargura = Mapa 8 ([(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])])
mapatestfailrio = Mapa 9 ([(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Rio (3), [Tronco,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])])
mapamapatest = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])
mapafail = Mapa 9 ([(Rio 2, [Tronco,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])])

mapatestFAIL1 :: Mapa
mapatestFAIL1 = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])
mapatestfailtipo2 = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Carro,Tronco]),(Estrada 2, [Nenhum,Carro])])
parteste = (Rio 6 ,[Tronco, Tronco, Tronco,Tronco, Nenhum , Tronco])

mapafailnonexhaust =  Mapa 3 [(Rio 2, [Nenhum,Tronco,Carro])]