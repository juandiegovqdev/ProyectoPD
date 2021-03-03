module TableroDeJuego
  ( colorCelda
  , numeroFilas
  , numeroCeldas
  , Tablero(..)
  , Fila(..)
  , Celda(..)
  , filaVacia
  , tableroVacio
  , filasATablero
  ) where

import Bloques ()
import Graphics.Gloss

-- Definición de celda.
data Celda
  = Empty
  | FilledWith Color
  deriving (Show, Eq)

-- Definición de fila.
data Fila =
  FilaDeCeldas [Celda]
  deriving (Show)

-- Deficinión del tablero.
data Tablero =
  TableroDeFilas [Fila]
  deriving (Show)

-- Devuelve el número de filas en el tablero.
numeroFilas :: Tablero -> [(Float, Fila)]
numeroFilas (TableroDeFilas filas) = zip [0 .. 21] filas

-- Devuelve el número de celdas por fila.
numeroCeldas :: Fila -> [(Float, Celda)]
numeroCeldas (FilaDeCeldas celdas) = zip [0 .. 9] celdas

-- Construye un tablero dadas las filas.
filasATablero :: [(Float, Fila)] -> Tablero
filasATablero x = TableroDeFilas (unZip x)

unZip :: [(Float, Fila)] -> [Fila]
unZip = foldr (\x -> (++) [snd x]) []

-- Devuelve el color de la celda.
colorCelda :: Celda -> Color
colorCelda Empty              = black
colorCelda (FilledWith color) = color

-- Devuelve una fila vacía.
filaVacia :: Fila
filaVacia = FilaDeCeldas (replicate 10 Empty)

-- Devuelve un tablero vacío.
tableroVacio :: Tablero
tableroVacio = TableroDeFilas (replicate 22 filaVacia)