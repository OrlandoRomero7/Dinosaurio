{--
INSTITUTO TECNOLOGICO DE ENSENADA
PROGRAMACION LOGICA Y FUNCIONAL

PROYECTO DE CIERRE DE SEMESTRE
JUEGO EN HASKELL

LORENA TAMAYO VAZQUEZ
17760235
--}

--Librerias para programa--
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color

--Definicion de tipos--

--Direccion movimiento deseado por el usuario--
data DireccionMov
  = Este
  | Oeste
  | None
  deriving (Eq)

--Vista del personaje--
data Vista
  = VerOeste
  | VerEste
  deriving (Eq)

-- Almacenamiento del Estado del Juego--
data EstadoJuego =
  EstadoJuego
    { posicion :: Point
    , direccion :: DireccionMov
    , viendo :: Vista
    , nivelJugando :: Nivel
    , contarSprite :: Int
    , velocidadX :: Float
    , velocidadY :: Float
    }

-- Definicion de tipos para celdas--

type TipoCelda = Char -- recibe el tipo segun el nivel(a para piso, b para moneda)
type Celda = (Point, TipoCelda) --Posicion x/y, el tipo de celda--
type Nivel = [Celda] --Lista de celdas--

-- Creacion de ventana--
ventana :: Display
ventana = InWindow "Juego con Haskell" (1300, 758) (0, 0)

--Definicion de fondo--
background :: Color
background = makeColor 0.0 0.0 0.1 1.0

--Definicion de los fotogramas por segundo--
fps :: Int
fps = 60

--Dibujar las filas para obtener nivel--
prepararDatos :: [String] -> Nivel
prepararDatos rawData =
  concat [hacerFila (rawData !! y) y | y <- [0 .. length rawData - 1]]

tamañoPiso :: Float
tamañoPiso = 30.0

hacerFila :: String -> Int -> Nivel
hacerFila row y =
  [ ( ( (fromIntegral x * tamañoPiso) - ((1300 / 2) - (tamañoPiso / 2))
      , (fromIntegral y * tamañoPiso) - ((758 / 2) - (tamañoPiso / 2)))
    , row !! x)| x <- [0 .. length row - 1], row !! x == 'a' || row !! x == 'b'
  ]

--Renderizar las imagenes del piso y monedas--
cualImagen :: Celda -> Picture -> Picture -> Picture
cualImagen (_, tipoCelda) piso moneda =
  if tipoCelda == 'a'
    then piso
    else moneda

dibujarObjeto :: Celda -> Picture -> Picture -> Picture
dibujarObjeto celda imagenPiso imagenMoneda =
  uncurry translate (fst celda) (cualImagen celda imagenPiso imagenMoneda)

render :: EstadoJuego -> [Picture] -> Picture
render gs imgs =
  pictures
    ([dibujarObjeto celda (head imgs) (imgs !! 1) | celda <- nivelJugando gs] ++
     [ translate
         (fst (posicion gs))
         (snd (posicion gs) + 10)
         (imgs !! (contarSprite gs + 2 + esDerecha (viendo gs)))
     ])

--Obtener la direccion de la vista para dibujar sprite de personaje--
esDerecha :: Vista -> Int
esDerecha VerEste = 6
esDerecha _ = 0

-- Definicion de cuatro estados para las entradas de teclado para obtener direcciones--
teclas :: Event -> EstadoJuego -> EstadoJuego
teclas (EventKey (SpecialKey KeyLeft) Down _ _) gs = gs {direccion = Oeste, viendo = VerOeste}
teclas (EventKey (SpecialKey KeyRight) Down _ _) gs = gs {direccion = Este, viendo = VerEste}
teclas (EventKey (SpecialKey KeyUp) Down _ _) gs =
  gs
    { velocidadY =
        if esColision gs (fst (posicion gs), snd (posicion gs) + velocidadY gs) 'a'
          then 6
          else (-6)
    }
teclas _ gs = gs {direccion = None}

-- Verificar la velocidad del personaje en eje Y--
verificarVelocidadY :: EstadoJuego -> Float
verificarVelocidadY gs
  | esColision gs (fst (posicion gs), snd (posicion gs) + velocidadY gs) 'a' = -3
  | velocidadY gs >= -6 = velocidadY gs - 0.1
  | otherwise = -6

-- Verificar la velocidad del personaje en eje X--
verificarVelocidadX :: EstadoJuego -> Float
verificarVelocidadX gs
  | direccion gs == Oeste || direccion gs == Este =
    if velocidadX gs > 5.0
      then 5.0
      else velocidadX gs + 0.5
  | otherwise =
    if velocidadX gs <= 0
      then 0
      else velocidadX gs - 0.5

-- Saber si se obtuvo un punto (devuelve true o false)--
esPunto :: Point -> Point -> Bool
esPunto (b1x, b1y) (b2x, b2y) = b1x - 10 < b2x + tamañoPiso && 
                                b1x + 50 - 10 > b2x && b1y < b2y + tamañoPiso && b1y + 54 > b2y

--Verificar que el personaje haya chocado con una pared o techo--
esColision :: EstadoJuego -> Point -> TipoCelda -> Bool
esColision gs pnt checkType =
  any (\((x, y), tileType) -> tileType == checkType && esPunto pnt (x, y))
      (nivelJugando gs)

-- Mover el personaje en eje X al este o oeste--
moverX :: DireccionMov -> EstadoJuego -> Point
moverX Este gs =
  if not (esColision gs (fst (posicion gs) + velocidadX gs, snd (posicion gs)) 'a')
    then (fst (posicion gs) + velocidadX gs, snd (posicion gs))
    else posicion gs
moverX Oeste gs =
  if not
       (esColision
          gs
          (fst (posicion gs) + velocidadX gs * (-1), snd (posicion gs))
          'a')
    then (fst (posicion gs) + velocidadX gs * (-1), snd (posicion gs))
    else posicion gs
moverX _ gs =
  if velocidadX gs > 0 &&
     not
       (esColision
          gs
          ( fst (posicion gs) +
            velocidadX gs *
            (if viendo gs == VerOeste
               then (-1)
               else 1)
          , snd (posicion gs))
          'a')
    then ( fst (posicion gs) +
           velocidadX gs *
           (if viendo gs == VerOeste
              then (-1)
              else 1)
         , snd (posicion gs))
    else posicion gs

-- Mover el personaje en eje Y arriba o abajo--
moverY :: EstadoJuego -> Point -> Point
moverY gs pnt =
  if not (esColision gs (fst pnt, snd pnt + velocidadY gs) 'a')
    then (fst pnt, snd pnt + velocidadY gs)
    else pnt

--Incrementar el sprite para recorrer a siguiente imagen de la lista--
incSprite :: EstadoJuego -> Int
incSprite gs =
  if direccion gs /= None
    then if contarSprite gs == 5
           then 0
           else contarSprite gs + 1
    else contarSprite gs

-- Saber si se a colectado una moneda--
esMoneda :: EstadoJuego -> Nivel
esMoneda gs =
  filter
    (\celda -> not (esPunto (fst celda) (posicion gs) && snd celda == 'b'))
    (nivelJugando gs)

-- Actualizar el estado de juego--
update :: Float -> EstadoJuego -> EstadoJuego
update _ gs =
  gs
    { velocidadY = verificarVelocidadY gs
    , velocidadX = verificarVelocidadX gs
    , posicion = moverY gs $ moverX (direccion gs) gs
    , contarSprite = incSprite gs
    , nivelJugando = esMoneda gs
    }



main :: IO ()
main = do
  putStrLn "Selecciona un nivel \n a)Facil \n b)Dificil"
  dificultad <- getChar
  let dif = if dificultad == 'a' then "assets/level" else if dificultad == 'b' then "assets/level2" else error "Escribe una opcion valida"
  rawData <- readFile dif
  imagenPiso <- loadBMP "assets/pared.bmp"
  imagenMoneda <- loadBMP "assets/bit.bmp"
  left1 <- loadBMP "assets/14i.bmp"
  left2 <- loadBMP "assets/13i.bmp"
  left3 <- loadBMP "assets/12i.bmp"
  left4 <- loadBMP "assets/11i.bmp"
  left5 <- loadBMP "assets/10i.bmp"
  left6 <- loadBMP "assets/9i.bmp"
  right1 <- loadBMP "assets/1p.bmp"
  right2 <- loadBMP "assets/2.bmp"
  right3 <- loadBMP "assets/3.bmp"
  right4 <- loadBMP "assets/4.bmp"
  right5 <- loadBMP "assets/5.bmp"
  right6 <- loadBMP "assets/6.bmp"
  let nivel = prepararDatos $ reverse $ lines rawData
  let state =
        EstadoJuego
          { posicion = (0.0, 0.0)
          , direccion = None
          , nivelJugando = nivel
          , contarSprite = 0
          , viendo = VerOeste
          , velocidadX = 0
          , velocidadY = (-6)
          }
  play
    ventana
    background
    fps
    state
    (`render` [ imagenPiso
              , imagenMoneda
              , left1
              , left2
              , left3
              , left4
              , left5
              , left6
              , right1
              , right2
              , right3
              , right4
              , right5
              , right6

              ])
    teclas
    update
