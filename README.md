# Proyecto TETRIS (HASKELL)

Trabajo realizado para la asignatura *Programación Declarativa*, en el cuál se pone en práctica los diferentes conceptos
adquiridos a lo largo del curso académico.

## Integrantes del Grupo

**Nombre**: Enrique García Velasco

**Nombre:** Juan Diego Villalobos Quirós

## Temática elegida.

Para la realización de este proyecto desde el profesorado nos sugirieron varias temáticas, tras las primeras reuniones
ambos integrantes del grupo teníamos más o menos claro cuál de ellas nos resultaban más atractivas: Ciencia del dato y
Juegos visuales avanzados. Nuestra primera opción fue ciencia del dato, buscamos algunos **datasets** relacionados con
criptomonedas y blockchain, pero cambiamos de temática al no encontrar ningún dataset/API que nos terminase de gustar.
Además, no teníamos demasiado claro cómo enfocar el proyecto. Debido a esto decidimos pasar a la segunda opción, el
desarrollo de un juego visual avanzado. Con esta temática ya elegida, las siguientes reuniones nos sirvieron para
definir la hoja de ruta del proyecto. Con la realización de un juego visual no pretendíamos solucionar ningún problema,
por ello nuestro principal objetivo es realizar un juego totalmente funcional y divertido. Tras varias opciones y
planteamientos de diferentes juegos nos decidimos por el clásico *Tetris*, pensamos que sería un juego medianamente
fácil de implementar y que a la misma vez nos permitiría cumplir e impclementar todos los requisitos necesarios en el
proyecto.

## Estructura

Nuestro proyecto cuenta con varios archivos diferentes, cada uno de estos archivos contienen funciones con una finalidad
diferentes, por ello vamos a explicar archivo por archivo que podemos encontrar en su interior y cuál seria su
funcionalidad.

### Bloques.hs

En este archivo podemos encontrar diferentes funciones, todas ellas relacionadas con la creación de los bloques de
juego. Una de las funciones más importantes de este archivo es la encargada de generar nuevos bloques con sus
coordenadas y un color. También contamos con las funciones de rotar Izq y Der, y con dos funciones que nos devuelven el
color y las coordenadas de un bloque.

### Config.hs

En este archivo es donde se define y se tiene la información del juegos como puede ser la puntuación, el tiempo, la
posición del bloque o si el juego ha finalizado entre otros. este archivo cuenta con una configuración inicial y final (
gameOver).

### Controles.hs

Este archivo es bastante importante ya que contamos con bastantes funciones en su interior, como su nombre indica todas
las funciones que encontramos en él están relacionadas con el manejo de los bloques durante la partida. En primer lugar
nos encontramos la funciones encargadas de mover los bloques a izquierda o derecha mediante la pulsación de flechas de
nuestro teclado además de otra función relacionada con la flecha hacía abajo para bajar nuestro bloque a una mayor
velocidad. A continuación tenemos las funciones de rotación, asignadas a las teclas 'a' para el giro en sentido
antihorario, y la tecla 'd' para la rotación en el sentido de la agujas del reloj. Además de todas estas contamos con
una función que comprueba si nos chocamos con algún borde/limite.

### GenerarTablero.hs

Tenemos unas primeras funciones que nos sirven para elegir el número de celdas, color del juego, altura del tablero o
los márgenes. Posteriormente encontramos multitud de funciones que se encargan del apartado gráfico del juego, nos
devuelven las coordenadas de cada celda, convierte las coordenadas a las coordenadas de la pantalla del juego, también
renderiza las celdas o mueve pone a los bloques como celdas temporales del tablero, todas estas configuraciones las
conseguimos pasar a una única imagen mediante una última función.

### Logica.hs

En este archivo tenemos todas la funciones relacionadas con la lógica del juego en sí, empezando por algo tan básico
como la velocidad del bloque, la actualización de la configuración, la eliminación de las filas que se van completando,
las colisiones entre bloques, colisiones con los bordes del tablero, las coordenadas de dichas colisiones o determinar
cuando el juego ha finalizado.

### TableroDeJuego.hs

Definimos un tablero vacío, el color de las celdas y las filas.

## Elementos mínimos exigidos

### Funciones básicas de prelude

##### Primer ejemplo

```  
celdasCoords :: Tablero -> [(Float, Float, Celda)]
celdasCoords tablero = concatMap extraerFilas (numeroFilas tablero)
  where
    extraerFilas :: (Float, Fila) -> [(Float, Float, Celda)]
    extraerFilas (y, fila) = map extraerCeldas (numeroCeldas fila)
      where
        extraerCeldas (x, c) = (x, y, c)
```

##### Segundo ejemplo

```  
numeroCeldas :: Fila -> [(Float, Celda)]
numeroCeldas (FilaDeCeldas celdas) = zip [0 .. 9] celdas
```

### Funciones básicas Data.list

##### Primer ejemplo (Logica.hs, línea 131)

```  
colision :: Float -> [(Float, Celda)] -> Bool
colision _ [] = False
colision yx x =
  if yx == fst (head x)
    then colorCelda (snd (head x)) /= black
    else colision yx ((tails x)!!1)
```

##### Segundo ejemplo (Logica.hs, línea 116)

```  
mapColision :: Config -> [(Float, Float)] -> Bool
mapColision _ [] = False
mapColision config x =
  haColisionadoEnCoords (head x) (numeroFilas (tableroJuego config)) ||
  mapColision config ((tails x)!!1)
```

### Funciones recursivas

##### Primer ejemplo

Logica.hs, línea 51

```  
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
```

##### Segundo ejemplo (Logica.hs, línea 63)

```  
eliminarFila :: [(Float, Fila)] -> Fila -> Float -> [(Float, Fila)]  
eliminarFila [] _ _ = []  
eliminarFila (x:xs) ultimaFila filaCompleta  
  | fst x > filaCompleta = x : xs  
  | fst x == 0 = (0, filaVacia) : eliminarFila xs (snd x) filaCompleta  
  | otherwise = (fst x, ultimaFila) : eliminarFila xs (snd x) filaCompleta
```

### Funciones por patrones

##### Primer ejemplo (TableroDeJuego.hs, línea 48)

```  
colorCelda :: Celda -> Color  
colorCelda Empty = black  
colorCelda (FilledWith color) = color
```

##### Segundo ejemplo (Logica.hs, línea 63)

```  
eliminarFila :: [(Float, Fila)] -> Fila -> Float -> [(Float, Fila)]  
eliminarFila [] _ _ = []  
eliminarFila (x:xs) ultimaFila filaCompleta  
  | fst x > filaCompleta = x : xs  
  | fst x == 0 = (0, filaVacia) : eliminarFila xs (snd x) filaCompleta  
  | otherwise = (fst x, ultimaFila) : eliminarFila xs (snd x) filaCompleta
```

### Usos de guardas

##### Primer ejemplo (Logica.hs, línea 84)

```  
noColision :: Config -> Bool  
noColision config  
  | snd (posicionBloque config) < 0 = True  
  | colisionInferiorTablero (listaCoordenadasBloque config) ||  
      mapColision config (listaCoordenadasBloque config) = False  
  | otherwise = True
```

##### Segundo ejemplo (Logica.hs, línea 100)

```  
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
```

### Usos de Case of

##### Primer ejemplo (Controles.hs, línea 29)

```  
pulsacionTeclas (EventKey (SpecialKey KeyDown) pos _ _) config =  
  case pos of  
  Down -> moverHaciaAbajo config  
    otherwise -> config
```

##### Segundo ejemplo (Bloques.hs, línea 31)

```  
nuevoBloque :: Double -> Bloque  
nuevoBloque r =  
  case truncate (r * 1000) `mod` 6 of  
  0 -> BloqueCoordenadas [(0, 0), (0, 1), (-1, 0), (-1, 1)] (dim red)  
    1 -> BloqueCoordenadas [(0, 0), (1, 0), (0, 1), (0, -1)] (dark orange)  
    2 -> BloqueCoordenadas [(0, 0), (1, 0), (2, 0), (-1, 0)] (dim blue)  
    3 -> BloqueCoordenadas [(0, 0), (1, 0), (0, -1), (-1, -1)] (dim cyan)  
    4 -> BloqueCoordenadas [(0, 0), (0, 1), (1, 0), (-1, 0)] (dim yellow)  
    5 -> BloqueCoordenadas [(0, 0), (0, -1), (-1, 0), (1, -1)] (dark (dim green))cyan)) 
```

### Usos de listas por comprensión

##### Primer ejemplo (Controles.hs, línea 52)

```  
colisionLimite :: [(Float, Float)] -> Bool  
colisionLimite xs = all (==True) [(not (fst x > 9 || fst x < 0)) | x <- xs]
```

##### Segundo ejemplo (Logica.hs, línea 71)

```  
filaCompleta :: [(Float, Celda)] -> Bool  
filaCompleta  xs = all (==True) [(colorCelda (snd x) /= black) | x <- xs]
```

### Usos de orden superior

##### Primer ejemplo (Controles.hs, línea 52)

```  
colisionLimite :: [(Float, Float)] -> Bool  
colisionLimite xs = all (==True) [(not (fst x > 9 || fst x < 0)) | x <- xs]
```

##### Segundo ejemplo (Logica.hs, línea 71)

```  
filaCompleta :: [(Float, Celda)] -> Bool  
filaCompleta  xs = all (==True) [(colorCelda (snd x) /= black) | x <- xs]
```

### Usos de evaluación perezosa

##### Primer ejemplo (Controles.hs, línea 46)

```  
puedeMover :: Config -> Bool  
puedeMover config =  
  colisionLimite (listaCoordenadasBloque config) &&  
  not (mapColision config (listaCoordenadasBloque config))
```

##### Segundo ejemplo (Controles.hs, línea 52)

```  
colisionLimite :: [(Float, Float)] -> Bool  
colisionLimite xs = all (==True) [(not (fst x > 9 || fst x < 0)) | x <- xs]
```

### Ejemplos de módulos desarrollados

##### Primer ejemplo (Bloques.hs)

```  
module Bloques  
  ( Bloque(..)  
  , existeBloqueEnCoordenadas  
  , colorBloque  
  , rotarBloqueDerecha  
  , rotarBloqueIzquierda  
  , nuevoBloque  
  , coordenadasBloque  
  ) where
```

##### Segundo ejemplo (Config.hs)

```  
module Config  
  ( configJuegoInicial  
  , juegoFinalizadoConfig  
  , Config(..)  
  ) where
```

##### Tercer ejemplo (Controles.hs)

```  
module Controles  
  ( pulsacionTeclas  
  , colisionLimite  
  ) where
```

##### Cuarto ejemplo (GenerarTablero.hs)

```  
module GenerarTablero  
  ( renderizarMuro  
  , renderizar  
  , mostrarBloque  
  , mostrarTablero  
  , celdasCoords  
  , redimCoords  
  , tamCelda  
  ) where
```

##### Quinto ejemplo (Logica.hs)

```  
module Logica  
  ( actualizarConfig  
  , listaCoordenadasBloque  
  , mapColision  
  , noColision  
  , filaCompleta  
  , convertir  
  , colisionInferiorTablero  
  ) where
```

##### Sexto ejemplo (TableroDeJuego.hs)

```  
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
```

### Declaraciones de tipos de datos y algunos de sus usos

##### Primer ejemplo (TableroDeJuego.hs, línea 17)

```  
 data Celda  
  = Empty  
  | FilledWith Color  
  deriving (Show, Eq)

numeroCeldas :: Fila -> [(Float, Celda)]  
numeroCeldas (FilaDeCeldas celdas) = zip [0 .. 9] celdas

```

##### Segundo ejemplo (TableroDeJuego.hs, línea 22)

```  
data Fila =  
  FilaDeCeldas [Celda]  
  deriving (Show)

numeroFilas :: Tablero -> [(Float, Fila)]  
numeroFilas (TableroDeFilas filas) = zip [0 .. 21] filas
```

##### Tercer ejemplo (TableroDeJuego.hs, línea 28)

```
data Tablero =  
  TableroDeFilas [Fila]  
  deriving (Show)

filasATablero :: [(Float, Fila)] -> Tablero  
filasATablero x = TableroDeFilas (unZip x)
```

##### Cuarto ejemplo (TableroDeJuego.hs, línea 17)

```  
data Config = Config  
  { puntuacion :: Integer -- Puntuación actual.  
  , tiempo :: Float -- Tiempo actual.  
  , tiempoUltimoBloque :: Float -- Tiempo desde la generación del último bloque.  
  , tiempoHastaSigMov :: Float -- Tiempo restante hasta el próximo movimiento.  
  , tableroJuego :: Tablero -- Tablero actual.  
  , bloque :: Bloque -- Bloque controlado por teclado.  
  , posicionBloque :: (Float, Float)  -- Posición del bloque actual.  
  , bloqueAleatorio :: StdGen -- Bloque aleatorio.  
  , juegoFinalizado :: Bool -- Estado de juego una vez hemos perdido.  
  } deriving (Show)

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
```

##### Quinto ejemplo (Bloque.hs, línea 14)

```
data Bloque =  
  BloqueCoordenadas [(Float, Float)] Color 
  deriving (Show)

colorBloque :: Bloque -> Color  
colorBloque (BloqueCoordenadas _ cl) = cl
```

### Usos de tipos de datos abstractos

##### Primer ejemplo (Bloque.hs, línea 46)

```  
rotarBloqueIzquierda :: Bloque -> Bloque  
rotarBloqueIzquierda (BloqueCoordenadas cords col) = BloqueCoordenadas (map rotarIzquierda cords) col  
  where  
  rotarIzquierda (a, b) = (b, -a)
```

##### Segundo ejemplo (GeneraTablero.hs, línea 73)

```  
mostrarTablero :: Tablero -> Picture  
mostrarTablero tablero = pictures $ map celdaAPic $ celdasCoords tablero  
  where  
  celdaAPic (x, y, celda)  
      | y < 1 = pictures []  
      | celda == Empty = pictures []  
      | otherwise = mostrarCelda (x, y) (colorCelda celda)
```

## Guía de uso

### ¿Cómo configurar el proyecto?

Para ejecutar el proyecto, es necesario instalar Stack. En dicha página podrás encontrar diversos instaladores; cada uno
especializado en un Sistema Operativo y arquitectura distintos.

Una vez hemos instalado Stack, podemos abrir el proyecto usando IntelliJ (en nuestro caso hemos usado este IDE). Además
de instalar IntelliJ, tenemos que instalar un plugin, el cual nos permite ejecutar código Haskell.

**NOTA: Los pasos relacionados con IntelliJ solo son necesarios en caso de que se quiera ejecutar el proyecto usando
este IDE. También es posible ejecutarlo usando Visual Studio Code. Aún así, es necesario instalar Stack.**

Una vez hemos abierto el proyecto usando IntelliJ, (o Visual Studio Code) tenemos que abrir una ventana de Terminal, con
el directorio situado en la carpeta del proyecto. Si usamos la terminal de IntelliJ, o Visual Studio Code, este paso no
es necesario (pues la terminal del IDE tendrá su *working directory* situado en el proyecto). Acto seguido, debemos de
ejecutar los siguientes procesos:

 ```
 stack install
 stack install gloss (o cabal install gloss en caso de que queramos usar este gestor de paquetes)
 ```

### ¿Cómo ejecutar el proyecto?

Una vez hayamos realizado todos los pasos anteriores, los únicos comandos que tenemos que ejecutar son:

```
stack build
stack exe ProyectoPD-exe
``` 

### Enlaces de descarga

Stack: https://docs.haskellstack.org/en/stable/install_and_upgrade/

IntelliJ: https://www.jetbrains.com/es-es/idea/download/#section=windows

[Stack]: https://docs.haskellstack.org/en/stable/install_and_upgrade/

[IntelliJ]: https://www.jetbrains.com/es-es/idea/download/#section=windows

## Librerías extras

La única librería externa que hemos utilizado durante el desarrollo del proyecto ha sido Gloss, una librería bastante
popular para hacer gráficos en haskell, nosotros hemos trabajado con stack durante este proyecto haciendo uso del
comando

```  
stack install gloss
``` 

aunque también se podría usar el siguiente comando para instalarla a través de cabal

```  
cabal install gloss
``` 

Web oficial Gloss (Hackage): https://hackage.haskell.org/package/gloss

