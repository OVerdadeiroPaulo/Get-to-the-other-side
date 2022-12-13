module Lixo where
import LI12223 
import LI12223 (Terreno(Relva))
{-import LI12223 (Terreno(Rio, Estrada, Relva))
deslocaobs :: Mapa -> Coordenadas -> Obstaculo
deslocaobs mapa@(Mapa l (((Rio vel, (h:t)):xs))) (a,b)
  |a == 0 = veobstaculonacoordenada (mapa) (l-(vel-1),b)
  | a == l = veobstaculonacoordenada mapa (0+(vel-1),b)
  |otherwise = veobstaculonacoordenada mapa (a+vel,b)
deslocaobs mapa@(Mapa l (((Estrada vel, (h:t)):xs))) (a,b)
  |a == 0 = veobstaculonacoordenada (mapa) (l-(vel-1),b)
  | a == l = veobstaculonacoordenada mapa (0+(vel-1),b)
  |otherwise = veobstaculonacoordenada mapa (a+vel,b)
deslocaobs mapa@(Mapa l ([(Relva, obs)])) (a,b) = veobstaculonacoordenada mapa (a,b)

-}

{-|funcao  que valida se na ha demasiados terrenos do mesmo tipo-}
{-terrenosseguidos :: Int -> Mapa -> Bool
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
-}
inicioantigo :: Show a => a -> [Char]
inicioantigo x =(take 3(show x))
inicionovo :: Terreno -> String
inicionovo (Rio vel) = "Rio"
inicionovo (Estrada  vel) = "Est" 
inicionovo Relva =  "Rel"
--  deriving (Show(Int-> Mapa))
obsnaonenhum :: Terreno -> Obstaculo
obsnaonenhum (Rio vel) = Tronco
obsnaonenhum (Estrada vel) = Carro
obsnaonenhum (Relva) = Arvore
proxobsval :: (Terreno,[Obstaculo]) -> [Obstaculo]
proxobsval par@(terr,[]) = [Nenhum, (obsnaonenhum terr)]
proximosObstaculosValidoscurto :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidoscurto _ (terr, []) = [Nenhum, (obsnaonenhum terr)]
proximosObstaculosValidoscurto n (te, (x:xs)) |  tipobscurto (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]
                                         |  tipobscurto (te, (x:xs)) && n > length (x:xs) = [Nenhum,obsnaonenhum te]                     
                                         |  tipobscurto (te, (x:xs)) && n > length (x:xs) && not ( elem Nenhum (x:xs)) = [Nenhum]
                                         | otherwise = []

tipobscurto :: (Terreno,[Obstaculo]) -> Bool
tipobscurto (_, []) = True
tipobscurto (te, (x:xs))
  | inicio te == "Rel" && x == Arvore || x == Nenhum = tipobscurto (te, xs) 
  | inicio te == "Rio" && x == Tronco || x == Nenhum = tipobscurto (te, xs)
  | inicio te == "Est" && x == Carro  || x == Nenhum = tipobscurto (te, xs)
  | otherwise = False





tipodeobs :: Mapa -> Bool
tipodeobs (Mapa larg ([]))= True
tipodeobs (Mapa larg (((terr, (xs)):ys))) 
  | tipodeauxANTIGA (Mapa larg (((terr, (xs)):ys))) == False = False
  | otherwise = tipodeauxANTIGA (Mapa larg ((ys)))



tipodeauxANTIGA :: Mapa -> Bool
tipodeauxANTIGA (Mapa l []) = True
tipodeauxANTIGA (Mapa l (((terr, []):ys))) = True
tipodeauxANTIGA (Mapa l (((terr, [x]):ys)))
  | inicio terr == "Rel" && (x == Carro || x== Tronco) = False
  | inicio terr == "Rio" && (x == Carro || x== Arvore) = False
  | inicio terr == "Est" && (x == Tronco || x == Arvore) = False
  | otherwise = True
tipodeauxANTIGA (Mapa l ([(terr, (x:xs))]))
  | inicio terr == "Rel" && ( x == Carro ||  x== Tronco) = False
  | inicio terr == "Est" && ( x == Arvore ||  x== Tronco) = False
  | inicio terr == "Rio" && ( x == Carro ||  x== Arvore) = False
  | otherwise = tipodeauxANTIGA (Mapa l [(terr, (xs))])

tipodeauxANTIGA (Mapa l (((terr, (x:xs)):ys)))
  | inicio terr == "Rel" && (x == Carro || x== Tronco) = False
  | inicio terr == "Rio" && (x == Carro || x== Arvore) = False
  | inicio terr == "Est" && (x == Tronco || x == Arvore) = False
  | otherwise = tipodeauxANTIGA (Mapa l ([(terr, (xs))]))

funcao= map (>5)( (map length (map (head == Tronco)(agrupaobs [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]))))
-- |funcoes nao usadas que usam o mapa para ver os Troncos, devido a complexidade preferi usar a outra opçao
outra x = not (elem True( map (possivel) (agrupaobs  x)))
possivel x = length x>5 && head x == Tronco 
{-|Versao antiga menos eficiente da funçao inicionovo-}
inicio :: Show a => a -> [Char]
inicio x =(take 3(show x))


contaobstaculos :: Mapa -> Bool
contaobstaculos (Mapa l (((terr, obs):xs))) 
 | contador obs == True = contaobstaculos (Mapa l ((xs))) 
 | otherwise = False
contador :: [Obstaculo] -> Bool
contador x = not (elem True( map (auxcontador) (agrupaobs  (x ++ x))))
auxcontador x = (length x>5 && head x == Tronco )|| (length x>3 && head x == Carro )

{-|juncao da veoscarros e veostroncos-}
obsemlinha :: Mapa -> Bool
obsemlinha (Mapa l ([])) = True
obsemlinha (Mapa l (((terr, obs):xs))) 
 | not (veoscarros (terr, obs) ) || not(veostroncos (terr, obs) )= False
 | otherwise = obsemlinha (Mapa l ((xs))) 
{-|funcao que valida o comprimento dos obstaculos(carros) -}
veoscarros :: (Terreno, [Obstaculo]) -> Bool
veoscarros (a, []) = True
veoscarros (a,[h,t]) = True
veoscarros vari@(terr,(h:t))
  | head x == Carro && length x > 3 = False
  | otherwise = veoscarros (terr,(t))
      where (x:xs) = agrupaobs ((h:t)++(h:t))

{-|Funcao que valida o comprimento dos obstaculos(troncos) -}
veostroncos :: (Terreno, [Obstaculo]) -> Bool
veostroncos (a, []) = True
veostroncos (a,[h,t]) = True
veostroncos vari@(terr,(h:t))
  | head x == Tronco && length x > 5 = False
  | otherwise = veostroncos ( terr,(t))
      where (x:xs) = agrupaobs ((h:t)++(h:t))


veosdois ::[Obstaculo] -> Int -> Bool
veosdois [] _ = True
veosdois  (h:t) n
  | head x == Carro && length x > n = False
  | otherwise = veosdois t n
      where (x:xs) = agrupaobs ((h:t)++(h:t))


obstseguidos :: Mapa -> Bool
obstseguidos (Mapa l []) = True
obstseguidos (Mapa l (((terr, (o:bs)):xs)))
  | inicionovo terr == "Est" && veosdois (o:bs) 3 == True = obstseguidos (Mapa l ((xs)))
  | inicionovo terr == "Est" && veosdois (o:bs) 3 ==  False = False
  | inicionovo terr == "Rio" && veosdois (o:bs) 5 == True = obstseguidos (Mapa l ((xs)))
  | inicionovo terr == "Rio" && veosdois (o:bs) 5 ==  False = False
  | inicionovo terr == "Relva" = True



contaNenhums :: (Terreno, [Obstaculo]) -> Jogador ->Int-> Int
contaNenhums par@(_,[])(Jogador (x,y)) acc = abs (acc -1)
contaNenhums par@(Estrada vel,o:bs) (Jogador (x,y)) acc
  | null obs =abs (acc -1)
  | Carro `notElem` (o:bs) = length (o:bs)
  | head obs == Nenhum =  abs (1 + contaNenhums (Estrada vel,(tail obs)) (Jogador (x,y)) (acc+1))
  | otherwise = abs (acc -1)
    where obs = if vel <= 0 then drop (x)(o:bs++o:bs) else   (reverse (drop (x+1) (o:bs++o:bs)))
exemplodeacumulador :: [Obstaculo] -> Int -> Bool
exemplodeacumulador [] k = True
exemplodeacumulador k 5 = False
exemplodeacumulador (x:xs) acc 
  | x == Arvore = exemplodeacumulador (xs ++ (x:xs)) (acc+1)
  |otherwise = exemplodeacumulador (xs ++(x:xs)) 0

veobstaculonacoordenada1 :: Mapa -> Coordenadas -> Obstaculo
veobstaculonacoordenada1 (Mapa l (((terr, [x]):ys))) (a,b) = x
veobstaculonacoordenada1 (Mapa l ([(x,y)])) (a,b) = last y
veobstaculonacoordenada1 (Mapa l (((terr, (x:xs)):ys))) (a,b) 
  | a == 0 && b == 0 = x
  | b== 0 && a /= 0 = veobstaculonacoordenada1 (Mapa l (((terr, (xs)):ys))) (a-1,b)
  | a == 0 && b /= 0 = veobstaculonacoordenada1 (Mapa l ((ys))) (a, b-1)
  | a /= 0 && b /= 0 = veobstaculonacoordenada1 (Mapa l ((ys))) (a-1, b-1)
