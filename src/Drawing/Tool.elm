module Tool exposing
  ( Tool
  , pencil
  , circle
  , line
  , rectangle
  , list
  , newPath
  )


import Path exposing (Path)
import Point exposing (Point)
import Color exposing (Color)


type Tool
  = Pencil
  | Circle
  | Line
  | Rectangle


pencil = Pencil


circle = Circle


line = Line


rectangle = Rectangle


list =
  [ Pencil, Circle, Line, Rectangle ]


newPath : Tool -> Path.Constructor
newPath tool =
  case tool of
    Pencil -> Path.pencil
    Line -> Path.line
    Circle -> Path.circle
    Rectangle -> Path.rectangle
