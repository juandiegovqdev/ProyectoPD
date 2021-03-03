module Main where

import Config
import Bloques
import GenerarTablero
import TableroDeJuego
import Logica
import Graphics.Gloss
import Controles

window :: Display
window = InWindow "ProyectoPD" (1920, 1080) (0, 0) -- Ahora mismo hemos puesto resolución Full HD (1920x1080)

background :: Color
background = black

main :: IO ()
main = play window black 60 configJuegoInicial renderizar pulsacionTeclas actualizarConfig

renderTMPFoo config = mostrarTablero tableroVacio