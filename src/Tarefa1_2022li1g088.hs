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

{-|Funcao principal que determina a validez de um mapa devolvendo True para um mapa válido e False para um mapa valido sendo que um mapa só é válido se todas as auxiliares devolverem True-}
mapaValido :: Mapa -> Bool
mapaValido mapa@(Mapa _ (((_, listadeobs):xs))) 
  | vervariosvazios mapa && vernrobstaculos mapa && tipodeobs mapa && riospostos mapa && obsemlinha mapa && terrenoscontiguos mapa = True
  | otherwise = False
{-|Funcao auxiliar que verifica que tem algum espaço com Nenhum obstaculo numa linha, que deVolve True quando encontra um nenhum e False se chegar ao fim da lista de obstaculos sem encontrar nenhum-}
vervazios :: (Terreno,[Obstaculo]) -> Bool
vervazios (terr,[x] ) 
  |x == Nenhum = True 
  | otherwise = False
vervazios (terr,(x:xs) )
  | x/= Nenhum = vervazios (terr,xs)
  |otherwise = True
{-|Funcao  que verifica que tem algum Terreno com Nenhum obstaculo num Mapa usando a vervazios, devolvendo False se encontrar algum terreno sem um nenhum e True se chegar ao fim da lista de pares sem dar false-}
vervariosvazios :: Mapa -> Bool
vervariosvazios (Mapa l [x]) 
  | vervazios x = True 
  |otherwise = False
vervariosvazios mapa@(Mapa l ((par@(terr, (o:bs)):xs))) 
  | vervazios par = vervariosvazios (Mapa l ((xs))) 
  |otherwise = False
{-|funcao que valida que a largura é do tamanho da lista de obstaculos, vendo recursivamente par a par se a length da lista de obstaculos é igual á largura do mapa, devolvendo False se encontrar uma lista com length diferente da largura e True se chagar ao fim da lista sem isto acontecer-}
vernrobstaculos :: Mapa -> Bool
vernrobstaculos (Mapa l []) = True
vernrobstaculos (Mapa l ((_ , k):xs)) 
  | l == length k = vernrobstaculos (Mapa l (xs))
  | otherwise = False
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
riospostos (Mapa larg ([(terr,x)])) = True
riospostos (Mapa larg ([])) = True
riospostos (Mapa larg (((Rio vel1, obst):(Rio vel2, obs):xs)))
  | vel1 * vel2 >= 0 = False
  | otherwise = riospostos (Mapa larg ((Rio vel2, obs):xs))
riospostos  (Mapa larg ((terr1, obst): (terr, obs):xs)) = riospostos (Mapa larg ((terr, obs):xs))
{-|auxiliar para veroscarrose verostroncos-}
agrupaobs :: Eq a => [a] -> [[a]]
agrupaobs [] = []
agrupaobs [x] = [[x]]
agrupaobs (x:xs) 
  | elem x (head a) = (x: (head a)) : tail a
  | otherwise = [x] : a
     where a = agrupaobs xs     
{-| Funcao que devolve False se encontrar algum sitio onde hajam 6 ou mais Troncos ou 4 ou mais Carros seguidos. 
ou se for possivél após deslocamento do mapa que os obstaculos fiquem seguidos. Usa a Contador como auxiliar que conta somente uma linha de obstaculos, que por sua vez faz map da auxcontador e procura um caso True , encontrrando um True dá False-}
obsemlinha :: Mapa -> Bool
obsemlinha (Mapa l []) = True
obsemlinha (Mapa l (((terr, obs):xs))) 
 | contador obs == True = obsemlinha (Mapa l ((xs))) 
 | otherwise = False
{-|devolve False se encorntrar Obstaculos seguidos acima de um numero n , ou false se nao existirem-}
contador :: [Obstaculo] -> Bool
contador x = not (elem True( map (auxcontador) (agrupaobs  (x ++ x))))
{-|auxiliar para contador que define n para cada obstaculo-}
auxcontador x = (length x>5 && head x == Tronco )|| (length x>3 && head x == Carro )
{-|Funcao que valida se ha 4 ou 5 terrenos contiguos dependendo do tipo de terreno devolvendo False se encontrar mais que 5 estradas ou relvas  ou mais que 4 rios seguidos. e false se chegar ao fim da lista sem isso acontecer-}
terrenoscontiguos :: Mapa -> Bool
terrenoscontiguos (Mapa _ (([]))) = True
terrenoscontiguos mapa@(Mapa l (((x):xs)))
  | (inicionovo (fst(head a)) == "Est" && length (a) >5 )|| (inicionovo (fst(head a))  == "Rel" && length (a)  > 5) = False
  | inicionovo (fst(head a)) == "Rio" && length (a) > 4 = False
  | otherwise = terrenoscontiguos (Mapa l (((xs))))
      where (a:b) = agrupaterrenos mapa
{-|auxiliar para terrenos contiguos que cria uma lista de listas pelo tipo de terreno similar á funcao group-}
agrupaterrenos :: Mapa -> [[(Terreno, [Obstaculo])]]
agrupaterrenos mapa@(Mapa _ (((terr, obst):xs))) = groupBy (\x y -> (elem (take 3(show x)) [take 3 (show  y)]))  ((terr, obst) : xs)
{-| funcao que devolve uma string com os primeiros 3 caracteres do Terreno para efeitos de comparaçao-}
inicionovo :: Terreno -> String
inicionovo (Rio vel) = "Rio"
inicionovo (Estrada  vel) = "Est" 
inicionovo Relva =  "Rel"
{-| funcao que devolve o tipo de obstaculo (excepto Nenhum) correspondente a um Terreno-}
obsnaonenhum :: Terreno -> Obstaculo
obsnaonenhum (Rio vel) = Tronco
obsnaonenhum (Estrada vel) = Carro
obsnaonenhum (Relva) = Arvore 
-- | mapa que devolve True em todas as auxiliares
verdadeiroemtodos = Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro])]
-- | mapa que devolve False na riosopostos
falsoNaVelocidadeOpostadosRios =Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (2), [Nenhum,Tronco])]
-- | mapa que devolve False na tipodeobs
falsoDevidoAoTipoDeObstaculos =  Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Carro])]
-- | mapa que devolve False na terrenoscontiguos devido ao nr de Rios
falsoemTerrenosContiguosRios = Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco])]
-- | mapa que devolve False na terrenoscontiguos devido ao nr de Estradas
falsoemTerrenosContiguosEstradas = Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Estrada 2, [Nenhum,Carro]),(Estrada 2, [Nenhum,Carro]),(Estrada 2, [Nenhum,Carro]),(Estrada 2, [Nenhum,Carro]),(Estrada 2, [Nenhum,Carro]),(Estrada  (-2), [Nenhum,Carro])]
-- | mapa que devolve False na terrenoscontiguos devido ao nr de Relvas
falsoemTerrenosContiguosRelva = Mapa 2 [(Rio 2, [Nenhum,Tronco]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore]),(Relva, [Nenhum,Arvore])]
-- | mapa que devolve False na vernrobstaculos
falsoNoComprimentodaLinha = Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Nenhum,Nenhum,Tronco])]
-- | mapa que devolve False na vervariosvazios
falsonosNenhums =Mapa 2 [(Estrada 2, [Nenhum,Carro]),(Rio 2, [Nenhum,Tronco]),(Rio (-2), [Tronco,Tronco])]
-- | mapa que devolve False na obsemlinha devido ao nr de Carros
falsoEmComprimentodeObsCarro  = Mapa 6 [(Estrada 2, [Carro,Carro,Nenhum,Carro,Carro,Carro]),(Rio 2, [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Rio (-2), [Nenhum,Tronco,Nenhum,Tronco,Nenhum,Tronco])]
-- | mapa que devolve False na obsemlinha devido ao nr de Troncos
falsoEmComprimentodeObsTronco  = Mapa 6 [(Estrada 2, [Carro,Nenhum,Nenhum,Carro,Nenhum,Carro]),(Rio 2, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco]),(Rio (-2), [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco])] 