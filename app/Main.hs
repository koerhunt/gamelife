module Main where

import Lib

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Concurrent

type GlPos = (GLfloat,GLfloat)
type GlBoard = [GlPos]

type Pos = (Int,Int)
type Board = [Pos]


gameConfig :: Board
gameConfig = [(20,10),(21,10),(22,10),(21,8),(20,9),(15,15),(16,15),(17,15)]

invertY :: Board -> Board
invertY xs = map (\(x,y) -> (x,-y)) xs

-- Convierte una lista de lista de tuplas a una lista de tuplas
joinList :: [GlBoard] -> GlBoard
joinList [] = []
joinList (x:xs) = x ++ joinList xs

width = 800
height = 800
ncells = 30
wcell =  ((truncate ncells)*2)
hcell =  ((truncate ncells)*2)

-- Incremento en X
dx :: GLfloat
dx = 1/(ncells)

-- Incremento en Y
dy :: GLfloat
dy = 1/(ncells)

-- Convertir una cordenada a los 4 puntos para dibujar
calculeQuad :: Pos -> GlBoard
calculeQuad (x,y) = [
  ((fromIntegral x*dx)-1,(fromIntegral y*dy)+1),
  ((fromIntegral x*dx)-1+dx,(fromIntegral y*dy)+1),
  ((fromIntegral x*dx)-1+dx,(fromIntegral y*dy)+1-dy),
  ((fromIntegral x*dx)-1,(fromIntegral y*dy)+1-dy)]


isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y),(x-1,y+1),
                          (x,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` wcell) +1, ((y-1) `mod` hcell +1))

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3] ]

births b = [p | p <- rmdups (concat (map neighbs b)),
 isEmpty b p,
 liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

main :: IO ()
main = do
    -- initializes the OpenGL systems
    (_progName, _args) <- getArgsAndInitialize
    -- opens the window
    _window <- createWindow "The LifeGame"
    -- controls the main display function
    windowPosition $= Position 0 0 --window position
    windowSize $= Size width height --windows size
    reshapeCallback $= Just reshape
    displayCallback $= display
    mainLoop

-- Limpiar buffer de pantalla
-- Draw contents
display :: DisplayCallback
display = do
  life gameConfig


life b = do
  clear [ ColorBuffer ]
  loadIdentity

  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  renderPrimitive Quads $ do
       color3f 1 0 0
       mapM_ (\(x, y) -> vertex $ Vertex2 x y ) (joinList (map calculeQuad (invertY (b)) ))
  renderPrimitive Lines $ do
       color3f 1 1 1
       mapM_ (\(x, y) -> vertex $ Vertex2 x y ) grid
  flush
  threadDelay 100000
  life $ nextgen b
  

-- Especificar el verdadero tamaño de la pantalla
reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, Size width height)

-- grid :: GLboard
grid = (joinList [ linev i | i <- [0..(ncells*2-1)] ]) ++ (joinList [ lineh i | i <- [0..(ncells*2-1)] ])

-- linev :: Int -> GLBoard
linev c = [ ((c*dx-1),1::GLfloat) ]++[ ((c*dx-1),-1::GLfloat) ]

lineh c = [ (1::GLfloat,(c*dy-1)) ]++[ (-1::GLfloat,(c*dy-1)) ]
