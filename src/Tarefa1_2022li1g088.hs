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
{-|Funcao principal que determina a validez de um mapa devolvendo True para um mapa válido e False para um mapa valido sendo que um mapa só é válido se todas as auxiliares devolverem True-}
mapaValido :: Mapa -> Bool
mapaValido mapa@(Mapa _ (((_, listadeobs):xs))) 
  | vervariosvazios mapa && vernrobstaculos mapa && tipodeobs mapa && riospostos mapa && obsemlinha mapa && terrenoscontiguos mapa = True
  | otherwise = False


{-|Funcao auxiliar que verifica que tem algum espaço com Nenhum obstaculo numa linha, que deVolve True quando encontra um nenhum e False se chegar ao fim da lista de obstaculos sem encontrar nenhum-}
vervazios :: (Terreno,[Obstaculo]) -> Bool
vervazios (terr,[]) = False
vervazios (terr,(x:xs) )
  | x== Nenhum = True
  |otherwise = vervazios (terr,xs)
{-|Funcao  que verifica que tem algum Terreno com Nenhum obstaculo num Mapa usando a vervazios, devolvendo False se encontrar algum terreno sem um nenhum e True se chegar ao fim da lista de pares sem dar false-}

vervariosvazios :: Mapa -> Bool
vervariosvazios mapa@(Mapa l ((par@(terr, (o:bs)):xs)))  = vervazios par ||  (vervariosvazios  (mapa))
                                                   where ((a,(y:ys)):ls) = xs
{-|funcao que valida que a largura é do tamanho da lista de obstaculos, vendo recursivamente par a par se a length da lista de obstaculos é igual á largura do mapa, devolvendo False se encontrar uma lista com length diferente da largura e True se chagar ao fim da lista sem isto acontecer-}



vernrobstaculos :: Mapa -> Bool
vernrobstaculos (Mapa l []) = True
vernrobstaculos (Mapa l ((_ , k):xs)) 
  | l == length k = vernrobstaculos (Mapa l (xs))
  | otherwise = False


obsnaonenhum :: Terreno -> Obstaculo
obsnaonenhum (Rio vel) = Tronco
obsnaonenhum (Estrada vel) = Carro
obsnaonenhum (Relva) = Arvore 
{-|Funcao auxiliar que verifica se o Terreno tem algum Obstaculo nao permitido, devolvendo False se encontrar algum obstaculo nao permitido ou True se chegar ao fim da lista sem isto acontecer.Tem um caso de excepçao para um Terreno so com um obstaculo e um mapa só com um Terreno -}


tipodeaux :: (Terreno,  [Obstaculo]) -> Bool
tipodeaux (terr, []) = True
tipodeaux (terr, [x])
  |(x /= Nenhum && x/= obsnaonenhum terr) = False
  | otherwise = True
tipodeaux (terr, (x:xs))
  |(x /= Nenhum && x/= obsnaonenhum terr) = False
  | otherwise = tipodeaux (terr, (xs))

{-|Funcao que valida se existe algum obstaculo invalido em varias linhas usando a tipodeauxANTIGA, devolvendo False se encontrar algum obstaculo inválido e True se chegar ao fim do mapa sem o encontrar. -}



tipodeobs :: Mapa -> Bool
tipodeobs (Mapa larg ([]))= True
tipodeobs (Mapa larg (((terr, (xs)):ys))) 
  | not (tipodeaux (terr, (xs))) = False
  | otherwise = tipodeobs (Mapa larg ((ys))) 
{-|Funcao que valida que rios contiguos tem velocidade oposta, devolvendo False se uma velocidade multiplicada pela outra for maior que 0 ou igual e True caso seja inferior a zero para todos os pares de rios. Devolve true tambem se aplicada a um terreno que nao seja Rio-}

riospostos :: Mapa -> Bool
riospostos (Mapa larg ([])) = True
riospostos (Mapa larg (((Rio vel1, obst):(Rio vel2, obs):xs)))
  | vel1 * vel2 < 0 = True
  | otherwise = riospostos (Mapa larg ((xs)))
riospostos _ = True
{-|Funcao que valida o comprimento dos obstaculos(troncos) -}


veostroncos :: (Terreno, [Obstaculo]) -> Bool
veostroncos (a, []) = True
veostroncos (a,[h,t]) = True
veostroncos vari@(Rio vel,(h:t))
  | head x == Tronco && length x > 5 = False
  | otherwise = veostroncos (Rio vel,(t))
      where (x:xs) = agrupaobs (h:t) ++ agrupaobs (h:t)
veostroncos _ = True
{-|auxiliar para veroscarrose verostroncos-}
agrupaobs :: Eq a => [a] -> [[a]]
agrupaobs [] = []
agrupaobs [x] = [[x]]
agrupaobs (x:xs) 
  | elem x (head a) = (x: (head a)) : tail a
  | otherwise = [x] : a
     where a = agrupaobs xs
     
{-funcoes nao usadas que usam o mapa para ver os Troncos, devido a complexidade preferi usar a outra opçao
outra x = not (elem True( map (possivel) (agrupaobs  x)))
possivel x = length x>5 && head x == Tronco -}
{-|funcao que valida o comprimento dos obstaculos(carros) -}
veoscarros :: (Terreno, [Obstaculo]) -> Bool
veoscarros (a, []) = True
veoscarros (a,[h,t]) = True
veoscarros vari@(Estrada vel,(h:t))
  | head x == Carro && length x > 5 = False
  | otherwise = veoscarros (Estrada vel,(t))
      where (x:xs) = agrupaobs (h:t) ++ agrupaobs (h:t)
veoscarros _ = True

{-|juncao da veoscarros e veostroncos-}
obsemlinha :: Mapa -> Bool
obsemlinha (Mapa l ([])) = True
obsemlinha (Mapa l (((terr, obs):xs))) 
 | not (veoscarros (terr, obs) ) || not(veostroncos (terr, obs) )= False
 | otherwise = obsemlinha (Mapa l ((xs))) 






{-|Funcao que valida se ha 4 ou 5 terrenos contiguos dependendo do tipo de terreno-}

terrenoscontiguos :: Mapa -> Bool
terrenoscontiguos (Mapa _ (([]))) = True
terrenoscontiguos mapa@(Mapa l (((x):xs)))
  | (inicionovo (fst(head a)) == "Est" && length (a) >5 )|| (inicionovo (fst(head a))  == "Rel" && length (a)  > 5) = False
  | inicionovo (fst(head a)) == "Rio" && length (a) > 4 = False
  | otherwise = terrenoscontiguos (Mapa l (((xs))))
      where (a:b) = agrupaterrenos mapa

{-|auxiliar para terrenos contiguos que cria uma lista de listas pelo tipo de terreno similar a group-}
agrupaterrenos :: Mapa -> [[(Terreno, [Obstaculo])]]
agrupaterrenos mapa@(Mapa _ (((terr, obst):xs))) = groupBy (\x y -> (elem (take 3(show x)) [take 3 (show  y)]))  ((terr, obst) : xs)
{-| funcao que devolve uma string com os primeiros 3 caracteres da lista -}
inicionovo :: Terreno -> String
inicionovo (Rio vel) = "Rio"
inicionovo (Estrada  vel) = "Est" 
inicionovo Relva =  "Rel"
{-|Versao antiga menos eficiente da funçao inicionovo-}
inicio :: Show a => a -> [Char]
inicio x =(take 3(show x))


mapatest = Mapa 4 [(Rio 2, [Nenhum,Tronco,Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])]
mapatest2 = Mapa 9 [(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])]
mapatestfailtipodeobs = Mapa 9 [(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum,Carro,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])]
mapatestfaillargura = Mapa 8 [(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])]
mapatestfailrio = Mapa 9 [(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Rio (-1), [Tronco,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])]
mapamapatest = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])
mapafail = Mapa 9 ([(Rio 2, [Tronco,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]),(Estrada 2, [Nenhum,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Nenhum,Nenhum]),(Relva, [Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum])])
mapaarvore = Mapa 2 ([(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore])])
mapatestFAIL1 = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])])
mapatestfailtipo2 = Mapa 2 ([(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Arvore,Carro])])
parteste = (Rio 6 ,[Tronco, Tronco, Tronco,Tronco, Nenhum , Tronco])
parteste2 = (Rio 6 ,[Nenhum, Tronco, Tronco,Tronco, Tronco , Tronco])
mapafailterrcontiguos = Mapa 2 ([(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore])])
mapafailnonexhaust =  Mapa 3 [(Rio 2, [Nenhum,Tronco,Carro])]

linhavervaz= [Tronco,Tronco,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum,Nenhum]
vervaziosfail = [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]
novoteste = Mapa 12 [(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
        (Estrada (-3),[Carro,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum]),
        (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore]),
        (Relva,[Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Nenhum,Nenhum,Arvore,Arvore]),
        (Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),
        (Rio (-5),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
        (Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),
        (Estrada 3,[Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Carro,Carro]),
        (Estrada (-3),[Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro]),
        (Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum]),
        (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum]),
        (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore]),
        (Rio (-5),[Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco]),
        (Rio 2,[Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum])]
falsoNaVelocidadeOpostadosRios =Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (2), [Nenhum,Tronco])]
falsoDevidoAoTipoDeObstaculos =  Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Carro])]
falsoemTerrenosContiguosRios = Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco])]
falsoNoComprimentodosObstaculos = Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (2), [Nenhum,Nenhum,Tronco])]