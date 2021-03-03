module Bloques
  ( Bloque(..)
  , existeBloqueEnCoordenadas
  , colorBloque
  , rotarBloqueDerecha
  , rotarBloqueIzquierda
  , nuevoBloque
  , coordenadasBloque
  ) where

import Graphics.Gloss

-- Objeto bloque
data Bloque =
  BloqueCoordenadas [(Float, Float)] Color
  deriving (Show)

-- Devuelve el color de bloque.
colorBloque :: Bloque -> Color
colorBloque (BloqueCoordenadas _ cl) = cl

-- Devuelve las coordenadas del bloque.
coordenadasBloque :: Bloque -> [(Float, Float)]
coordenadasBloque (BloqueCoordenadas cor _) = cor

-- Dadas unas coordenadas, comprueba si existe un bloque.
existeBloqueEnCoordenadas :: (Float, Float) -> Bloque -> Bool
existeBloqueEnCoordenadas coordenadas (BloqueCoordenadas coordenadas' _) = coordenadas `elem` coordenadas'

-- Genera un bloque nuevo.
nuevoBloque :: Double -> Bloque
nuevoBloque r =
  case truncate (r * 1000) `mod` 6 of
    0 -> BloqueCoordenadas [(0, 0), (0, 1), (-1, 0), (-1, 1)] (dim red)
    1 -> BloqueCoordenadas [(0, 0), (1, 0), (0, 1), (0, -1)] (dark orange)
    2 -> BloqueCoordenadas [(0, 0), (1, 0), (2, 0), (-1, 0)] (dim blue)
    3 -> BloqueCoordenadas [(0, 0), (1, 0), (0, -1), (-1, -1)] (dim cyan)
    4 -> BloqueCoordenadas [(0, 0), (0, 1), (1, 0), (-1, 0)] (dim yellow)
    5 -> BloqueCoordenadas [(0, 0), (0, -1), (-1, 0), (1, -1)] (dark (dim green))

rotarBloqueDerecha :: Bloque -> Bloque
rotarBloqueDerecha (BloqueCoordenadas cords col) = BloqueCoordenadas (map rotarDerecha cords) col
  where
    rotarDerecha (a, b) = (-b, a)

rotarBloqueIzquierda :: Bloque -> Bloque
rotarBloqueIzquierda (BloqueCoordenadas cords col) = BloqueCoordenadas (map rotarIzquierda cords) col
  where
    rotarIzquierda (a, b) = (b, -a)
