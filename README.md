# gamelife

El juego de la vida es un autómata celular diseñado por el matemático británico John Horton Conway en 1970. 
Se trata de un juego de cero jugadores, lo que quiere decir que su evolución está determinada por el estado inicial y no necesita ninguna entrada de datos posterior.

# prerequisitos

Para construir el proyecto y manejar dependencias se uso [stack](https://docs.haskellstack.org/en/stable/README/)stack
por lo cual debera estar instalado en el equipo.

Instalar los modulos OpenGL y GLUT
```stack install OpenGL```
```stack install GLUT```

# construir y ejecutar

```stack build```
```stack run```

# Cambio de los parametros para el juego

```
type Pos = (Int,Int)
type Board = [Pos]
gameConfig :: Board
```
Las celulas vivas estan declaradas en *gameConfig* representadas por un conjunto de coordenadas enteras, donde (0,0) es la esquina superior izquierda.

```
width = 800
height = 800
```
width y height especifican el ancho y alto de la pantalla en pixeles.

```
ncells
```
especifica el numero de celulas que pueden estar contenidas a lo ancho en un cuadrante, el tablero esta dividido en cuatro cuadrantes, si tenemos ncells = 30, tendremos un tablero de 60x60
