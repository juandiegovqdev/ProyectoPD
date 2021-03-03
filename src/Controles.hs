module Controles
  ( pulsacionTeclas
  , colisionLimite
  ) where

import Bloques
import Logica
import Graphics.Gloss.Interface.Pure.Game
import Config

pulsacionTeclas :: Event -> Config -> Config
-- Si pulsamos en la flecha izquierda, movemos el bloque hacia la izquierda.
pulsacionTeclas (EventKey (SpecialKey KeyLeft) pos _ _) config =
  if puedeMover (config {posicionBloque = (x - 1, y)}) && pos == Down
    then config {posicionBloque = (x', y)}
    else config
  where
    (x, y) = posicionBloque config
    x' = x - 1
-- Si pulsamos en la flecha derecha, movemos el bloque hacia la derecha.
pulsacionTeclas (EventKey (SpecialKey KeyRight) pos _ _) config =
  if puedeMover (config {posicionBloque = (x + 1, y)}) && pos == Down
    then config {posicionBloque = (x', y)}
    else config
  where
    (x, y) = posicionBloque config
    x' = x + 1
-- Si pulsamos en la fecha inferior, movemos el bloque hasta el final del ejeX.
pulsacionTeclas (EventKey (SpecialKey KeyDown) pos _ _) config =
  case pos of
    Down      -> moverHaciaAbajo config
    otherwise -> config
-- Si pulsamos la tecla d, movemos el bloque en sentido horario.
pulsacionTeclas (EventKey (Char 'd') pos _ _) config =
  if pos == Down && puedeMover (rotacionHoraria config)
    then rotacionHoraria config
    else config
-- Si pulsamos la tecla a, movemos el bloque en sentido antihorario.
pulsacionTeclas (EventKey (Char 'a') pos _ _) config =
  if pos == Down && puedeMover (rotacionAntihoraria config)
    then rotacionAntihoraria config
    else config
pulsacionTeclas _ config = config

--  Nos devuelve True si podemos actualizar la configuración (si podemos mover un bloque)
puedeMover :: Config -> Bool
puedeMover config =
  colisionLimite (listaCoordenadasBloque config) &&
  not (mapColision config (listaCoordenadasBloque config))

-- Comprueba que un bloque no ha colisionado con un límite.
colisionLimite :: [(Float, Float)] -> Bool
colisionLimite xs = all (==True) [(not (fst x > 9 || fst x < 0)) | x <- xs]

moverHaciaAbajo :: Config -> Config
moverHaciaAbajo config = config {posicionBloque = (x, y + n)}
  where
    (x, y) = posicionBloque config
    n = contarBloquesHaciaAbajo config 0

contarBloquesHaciaAbajo :: Config -> Float -> Float
contarBloquesHaciaAbajo config n
  | noColision (config {posicionBloque = (x, y + n)}) = contarBloquesHaciaAbajo config (n + 1)
  | otherwise = n - 1
  where
    (x, y) = posicionBloque config

rotacionHoraria :: Config -> Config
rotacionHoraria config = config {bloque = rotarBloqueDerecha $ bloque config}

rotacionAntihoraria :: Config -> Config
rotacionAntihoraria config = config {bloque = rotarBloqueIzquierda $ bloque config}
