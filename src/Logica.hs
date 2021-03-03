module Logica
  ( actualizarConfig
  , listaCoordenadasBloque
  , mapColision
  , noColision
  , filaCompleta
  , convertir
  , colisionInferiorTablero
  ) where

import Bloques
import GenerarTablero
import TableroDeJuego
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort ()
import Config
import Data.List
import System.Random

-- Velocidad horizontal
velocidadBloque :: Float
velocidadBloque = 4

-- Velocidad vertical
tiempoMovBloque :: Config -> Float
tiempoMovBloque config
  | juegoFinalizado config = 0
  | otherwise = 1.5 / velocidadBloque

actualizarConfig :: Float -> Config -> Config
actualizarConfig tm config = actualizaConfig config {tiempo = tiempo config + tm, tiempoUltimoBloque = tm}

-- Dada una configuración, actualizar la actual.
actualizaConfig :: Config -> Config
actualizaConfig config
  | juegoFinalizado config = juegoFinalizadoConfig {puntuacion = puntuacion config, tableroJuego = tableroJuego config}
  | tiempoHastaSigMov (actualizarTiempo config) <= 0 =
    moverBloque (actualizarTiempo config) {tiempoHastaSigMov = tiempoMovBloque config}
  | otherwise = config {tiempoHastaSigMov = tiempoHastaSigMov config - tiempoUltimoBloque config}
  where
    actualizarTiempo s =
      config {tiempoHastaSigMov = tiempoHastaSigMov config - tiempoUltimoBloque s}

-- Elimina las filas completas, actualizando la configuración.
eliminarFilasCompletas :: Config -> Config
eliminarFilasCompletas config = comprobarTablero (numeroFilas (tableroJuego config)) config

-- Elimina las filas vacías.
comprobarTablero :: [(Float, Fila)] -> Config -> Config
comprobarTablero [] config = config
comprobarTablero (x:xs) config =
  if filaCompleta (numeroCeldas (snd x))
    then eliminarFilasCompletas
           (config
            { puntuacion = puntuacion'
            , tableroJuego =
                filasATablero
                  (eliminarFila (numeroFilas (tableroJuego config)) filaVacia (fst x))
            })
    else comprobarTablero xs config
  where
    puntuacion' = puntuacion config + 100

eliminarFila :: [(Float, Fila)] -> Fila -> Float -> [(Float, Fila)]
eliminarFila [] _ _ = []
eliminarFila (x:xs) ultimaFila filaCompleta
  | fst x > filaCompleta = x : xs
  | fst x == 0 = (0, filaVacia) : eliminarFila xs (snd x) filaCompleta
  | otherwise = (fst x, ultimaFila) : eliminarFila xs (snd x) filaCompleta

-- Comprobar si una fila está completa.
filaCompleta :: [(Float, Celda)] -> Bool
filaCompleta  xs = all (==True) [(colorCelda (snd x) /= black) | x <- xs]

moverBloque :: Config -> Config
moverBloque config =
  if noColision config {posicionBloque = (x, y + 1)}
    then config {posicionBloque = (x, y')}
    else eliminarFilasCompletas $ cargarNuevoConfig config
  where
    (x, y) = posicionBloque config
    y' = y + 1

-- Comprobar que el bloque no ha colisionado.
noColision :: Config -> Bool
noColision config
  | snd (posicionBloque config) < 0 = True
  | colisionInferiorTablero (listaCoordenadasBloque config) ||
      mapColision config (listaCoordenadasBloque config) = False
  | otherwise = True

-- Devuelve True si el bloque ha colisionado en el final del tablero.
colisionInferiorTablero :: [(Float, Float)] -> Bool
colisionInferiorTablero [] = False
colisionInferiorTablero (x:xs) = resultado || colisionInferiorTablero xs
  where
    resultado = snd x > 21

-- Cuando colisiona un blque, tenemos que cargar una nueva configuración del juego con el nuevo bloque situado en las coordenadas
-- en las que ha colisionado.
cargarNuevoConfig :: Config -> Config
cargarNuevoConfig config
  | noColision (comenzarJuegoConfig {posicionBloque = posicionBloque config }) = configJuegoInicial
  | haFinalizadoJuego config = juegoFinalizadoConfig {puntuacion = puntuacion config, tableroJuego = tableroJuego config}
  | otherwise = comenzarJuegoConfig
    where comenzarJuegoConfig = config
                { posicionBloque = (4, 0)
                , tableroJuego = mostrarBloque (bloque config) (posicionBloque config) (tableroJuego config)
                , bloque = nuevoBloque $ fst nuevoBloqueAleatorio
                , bloqueAleatorio = snd nuevoBloqueAleatorio
                , juegoFinalizado = False
                }
                  where
                    nuevoBloqueAleatorio = randomR (0.0, 1.0) (bloqueAleatorio config)

-- Devuelve si existe alguna colisión.
mapColision :: Config -> [(Float, Float)] -> Bool
mapColision _ [] = False
mapColision config x =
  haColisionadoEnCoords (head x) (numeroFilas (tableroJuego config)) ||
  mapColision config ((tails x)!!1)

-- Dadas unas coordenadas, y una lista de coordenadas, si hay colisión en dichas coordenadas.
haColisionadoEnCoords :: (Float, Float) -> [(Float, Fila)] -> Bool
haColisionadoEnCoords _ [] = False
haColisionadoEnCoords (ejeX, ejeY) x =
  if fst (head x) == ejeY
    then colision ejeX (numeroCeldas (snd (head x)))
    else haColisionadoEnCoords (ejeX, ejeY) (tail x)

colision :: Float -> [(Float, Celda)] -> Bool
colision _ [] = False
colision yx x =
  if yx == fst (head x)
    then colorCelda (snd (head x)) /= black
    else colision yx ((tails x)!!1)

listaCoordenadasBloque :: Config -> [(Float, Float)]
listaCoordenadasBloque config = convertirCoordenadas (coordenadasBloque $ bloque config) config

convertirCoordenadas :: [(Float, Float)] -> Config -> [(Float, Float)]
convertirCoordenadas [] _ = []
convertirCoordenadas x config = convertir (head x) (posicionBloque config) : convertirCoordenadas (tail x) config

-- Devuelve la nueva posición.
convertir :: (Float, Float) -> (Float, Float) -> (Float, Float)
convertir (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

-- Comprueba si hemos perdido la partida.
haFinalizadoJuego :: Config -> Bool
haFinalizadoJuego config = comprobarPrimeraFila (numeroCeldas (snd (head (numeroFilas (tableroJuego config)))))

comprobarPrimeraFila :: [(Float,Celda)] -> Bool
comprobarPrimeraFila = foldr (\ x -> (||) (colorCelda (snd x) /= black)) False