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
inicio :: Show a => a -> [Char]
inicio x =(take 3(show x))
depara :: Terreno -> String
depara (Rio vel) = "Rio"
depara (Estrada  vel) = "Est" 
depara Relva =  "Rel"
--  deriving (Show(Int-> Mapa))
obsnaonenhum :: Terreno -> Obstaculo
obsnaonenhum (Rio vel) = Tronco
obsnaonenhum (Estrada vel) = Carro
obsnaonenhum (Relva) = Arvore
proxobsval :: (Terreno,[Obstaculo]) -> [Obstaculo]
proxobsval par@(terr,[]) = [Nenhum, (obsnaonenhum terr)]
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (terr, []) = [Nenhum, (obsnaonenhum terr)]
proximosObstaculosValidos n (te, (x:xs)) | inicio te == "Rio" && tipobs (te, (x:xs)) && (n-1) == length (x:xs) && not (elem Nenhum (x:xs)) = [Nenhum]
                                         | inicio te == "Rio" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum,Tronco]                     
                                         | inicio te == "Rel" && tipobs (te, (x:xs)) && n > length (x:xs) && not ( elem Nenhum (x:xs)) = [Nenhum]
                                         | inicio te == "Rel" && tipobs (te, (x:xs)) && n > length (x:xs) = [Nenhum, Arvore]
                                         | inicio te == "Est" && tipobs (te, (x:xs)) && 
                                         n > length (x:xs) && not ( elem Nenhum (x:xs)) = [Nenhum]
                                         | inicio te == "Est" && tipobs (te, (x:xs)) && 
                                         n > length (x:xs) = [Nenhum, Carro]
                                         | otherwise = []