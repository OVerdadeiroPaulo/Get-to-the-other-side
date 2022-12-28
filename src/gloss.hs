import Graphics.Gloss
import LI12223
displayMapa :: Mapa -> Picture
displayMapa mapa = Pictures [displayLinha mapa i | i <- [0..largura mapa - 1]]

displayLinha :: Mapa -> Int -> Picture
displayLinha mapa i = Pictures [displayTerreno mapa i, displayObstaculo mapa i j | j <- [0..length (snd (mapaObstaculos mapa !! i)) - 1]]

displayTerreno :: Mapa -> Int -> Picture
displayTerreno mapa i = translate 0 (fromIntegral i * tileSize) $ case fst (mapaObstaculos mapa !! i) of
    Rio -> bitmapOfBMP "rio.bmp"
    Estrada -> bitmapOfBMP "estrada.bmp"
    Relva -> bitmapOfBMP "relva.bmp"

displayObstaculo :: Mapa -> Int -> Int -> Picture
displayObstaculo mapa i j = translate (fromIntegral j * tileSize) (fromIntegral i * tileSize) $ case (snd (mapaObstaculos mapa !! i)) !! j of
    Nenhum -> blank
    Tronco -> bitmapOfBMP "tronco.bmp"
    Carro -> bitmapOfBMP "carro.bmp"
    Arvore -> bitmapOfBMP "arvore.bmp"

tileSize :: Float
tileSize = 32.0

main :: IO ()
main = do
  let mapa = Mapa 3 ([(Relva, [Nenhum,Arvore,Nenhum]),(Relva, [Arvore,Nenhum,Arvore]),(Relva, [Nenhum,Arvore,Nenhum])])
  let picture = displayMapa mapa
  simulate window background fps state render update
  where
    window = InWindow "My Window" (800, 600) (100, 100) True False
    background = white
    fps = 60
    state = mapa
    render _ = picture
    update _ = mapa

-- the rest of the code, including the displayMapa function, remains unchanged