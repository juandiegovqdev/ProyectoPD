module Config
  ( configJuegoInicial
  , juegoFinalizadoConfig
  , Config(..)
  ) where

import Bloques
import TableroDeJuego
import System.Random

-- Configuración de juego actual.
data Config = Config
  { puntuacion :: Integer             -- Puntuación actual.
  , tiempo :: Float                   -- Tiempo actual.
  , tiempoUltimoBloque :: Float       -- Tiempo desde la generación del último bloque.
  , tiempoHastaSigMov :: Float        -- Tiempo restante hasta el próximo movimiento.
  , tableroJuego :: Tablero           -- Tablero actual.
  , bloque :: Bloque                  -- Bloque controlado por teclado.
  , posicionBloque :: (Float, Float)  -- Posición del bloque actual.
  , bloqueAleatorio :: StdGen         -- Bloque aleatorio.
  , juegoFinalizado :: Bool           -- Estado de juego una vez hemos perdido.
  } deriving (Show)

-- Configuración inicial del juego.
configJuegoInicial :: Config
configJuegoInicial =
  Config
  { puntuacion = 0
  , tiempo = 0
  , tiempoUltimoBloque = 0
  , tiempoHastaSigMov = 0
  , tableroJuego = tableroVacio
  , posicionBloque = (5, 0)
  , bloque = nuevoBloque 1
  , bloqueAleatorio = mkStdGen 0
  , juegoFinalizado = False
  }

-- Configuración de juego cuando perdemos.
juegoFinalizadoConfig :: Config
juegoFinalizadoConfig =
  Config
  { puntuacion = 666
  , tiempo = 0
  , tiempoUltimoBloque = 0
  , tiempoHastaSigMov = 666
  , tableroJuego = tableroVacio
  , posicionBloque = (0,0)
  , bloque = nuevoBloque 1
  , bloqueAleatorio = mkStdGen 0
  , juegoFinalizado = True
  }
