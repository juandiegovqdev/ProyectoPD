module GenerarTablero
  ( renderizarMuro
  , renderizar
  , mostrarBloque
  , mostrarTablero
  , celdasCoords
  , redimCoords
  , tamCelda
  ) where

import Bloques
import TableroDeJuego
import Graphics.Gloss
import Config

-- Tamaño de la celda.
tamCelda :: Float
tamCelda = 32

-- Color del borde (los límites).
colorBorde :: Color
colorBorde = greyN 0.3

-- Anchura del borde.
anchuraBorde :: Float
anchuraBorde = 10 * tamCelda

-- Altura del borde.
alturaTablero :: Float
alturaTablero = 21 * tamCelda

-- Padding del tablero.
padding :: Float
padding = 20

colorTablero :: Color
colorTablero = white

-- Redimensiona las coordenadas (bloques) dados los tamaños.
redimCoords :: (Float, Float) -> (Float, Float)
redimCoords (x1, y1) = (x2, y2)
  where
    x2 = x1 * tamCelda - 5 * tamCelda
    y2 = 11 * tamCelda - (y1 * tamCelda)

renderizarMuro :: Picture
renderizarMuro =
  pictures
    [ translate (-tamCelda / 2) 0 $color colorBorde $
      rectangleSolid (anchuraBorde + padding) (alturaTablero + padding)
    , translate (-tamCelda / 2) 0 $
      color colorTablero $ rectangleSolid anchuraBorde alturaTablero
    ]

-- Devuelve una lista del celdas con sus respectivas coordenadas.
celdasCoords :: Tablero -> [(Float, Float, Celda)]
celdasCoords tablero = concatMap extraerFilas (numeroFilas tablero)
  where
    extraerFilas :: (Float, Fila) -> [(Float, Float, Celda)]
    extraerFilas (y, fila) = map extraerCeldas (numeroCeldas fila)
      where
        extraerCeldas (x, c) = (x, y, c)

-- Devuelve la celda renderizada.
mostrarCelda :: (Float, Float) -> Color -> Picture
mostrarCelda (x, y) col = translate x' y' $ color col $ rectangleSolid tam' tam'
  where
    x' = fst $ redimCoords (x, y)
    y' = snd $ redimCoords (x, y)
    tam' = tamCelda * 0.8

-- Devuelve el tablero renderizado.
mostrarTablero :: Tablero -> Picture
mostrarTablero tablero = pictures $ map celdaAPic $ celdasCoords tablero
  where
    celdaAPic (x, y, celda)
      | y < 1 = pictures []
      | celda == Empty = pictures []
      | otherwise = mostrarCelda (x, y) (colorCelda celda)

-- Muestra el bloque renderizado dadas las coordenadas y el tablero.
mostrarBloque :: Bloque -> (Float, Float) -> Tablero -> Tablero
mostrarBloque bloque (x, y) tablero = TableroDeFilas $ map renderizarFila $ numeroFilas tablero
  where
    renderizarFila (yP, fila) = FilaDeCeldas $ map mostrarCeldasEnFilas (numeroCeldas fila)
      where
        mostrarCeldasEnFilas (xP, celda)
          | celda /= Empty = celda
          | existeBloqueEnCoordenadas (xP - x, yP - y) bloque = FilledWith (colorBloque bloque)
          | otherwise = Empty

-- Renderiza la configuración del juego.
renderizar :: Config -> Picture
renderizar config
  | juegoFinalizado config =  pictures [bordes, tableroActual, mensajeJuegoFinalizado, puntuacionJuegoFinalizado]
  | otherwise = pictures [bordes, tableroActual, bloqueActivo,puntuacionJugador]
    where
      bordes = renderizarMuro
      tableroActual = mostrarTablero $ tableroJuego config
      bloqueActivo =
        mostrarTablero $ mostrarBloque (bloque config) (posicionBloque config) (tableroJuego config)
      mensajeJuegoFinalizado = translate (-200.0) 50.0 $ scale 0.5 0.5 (pictures [color (light rose) (Text "¡Has perdido!")])
      puntuacionJuegoFinalizado = translate (-250.0) (-50.0) $ scale 0.5 0.5 (pictures [color (light rose) (Text ("Puntuacion: " ++ show(puntuacion config)))])
      puntuacionJugador = translate 200.0 200.0 (scale 0.2 0.2 (pictures [puntuacionJugadorTexto]))
        where
          puntuacionJugadorTexto = color white (Text puntuacionTexto)
          puntuacionTexto = "Puntuacion: " ++ show (puntuacion config)