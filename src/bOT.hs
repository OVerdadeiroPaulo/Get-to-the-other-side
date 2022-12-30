module Bot where 
import LI12223

import System.Random (randomRIO)

navigate :: Jogo -> IO Jogada
navigate jogo = do
    -- Get the player's current position and the game map
    let jogador = jogoJogador jogo
        mapa = jogoMapa jogo

    -- Check if the player is already at (_, 0)
    if snd (coordenadas jogador) == 0
        then return Parado
        else do
            -- Check if the player is on a line with an obstacle that they need to avoid
            let linha = snd (mapaObstaculos mapa !! snd (coordenadas jogador))
            if fst linha == Estrada || (fst linha == Rio && Nenhum `elem` snd linha)
                then do
                    -- If the player is on a line with a Carro or a Rio without a Tronco, they need to move to a different line
                    let possiveisMovimentos = [Cima | snd (coordenadas jogador) > 0, fst (mapaObstaculos mapa !! (snd (coordenadas jogador) - 1)) /= Estrada && (fst (mapaObstaculos mapa !! (snd (coordenadas jogador) - 1)) /= Rio || Tronco `elem` snd (mapaObstaculos mapa !! (snd (coordenadas jogador) - 1)))]
                            ++ [Baixo | snd (coordenadas jogador) < largura mapa - 1, fst (mapaObstaculos mapa !! (snd (coordenadas jogador) + 1)) /= Estrada && (fst (mapaObstaculos mapa !! (snd (coordenadas jogador) + 1)) /= Rio || Tronco `elem` snd (mapaObstaculos mapa !! (snd (coordenadas jogador) + 1)))]
                    if null possiveisMovimentos
                        then return Parado
                        else do
                            -- If there is at least one valid option, return a random one
                            index <- randomRIO (0, length possiveisMovimentos - 1)
                            return (Move (possiveisMovimentos !! index))
                else do
                    -- If the player is not on a line with an obstacle, they can move towards (_, 0)



daavolta :: Jogador -> Jogada -> Mapa -> Mapa
daavolta jog@(Jogador (a,b)) gada mapa@(Mapa l (((terr, x:xs):ys)))
  | (x:xs) !! a == Carro && b == 0 =
      let newMapa = daavolta jog gada (Mapa l (((terr ,(x:xs)) :(desmapa ( daavolta  (Jogador (a,b-1))  gada (emmapa ((ys))))))))
      in Mapa l (((terr ,(x:xs)) :(desmapa newMapa)))
  | otherwise = mapa

