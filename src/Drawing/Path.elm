module Path exposing
  ( Path
  , Constructor
  , pencil
  , line
  , circle
  , rectangle
  , null
  , update
  , toCollageForm
  )


import Collage exposing (defaultLine)
import Element

import Point exposing (Point)
import Color exposing (Color)


type alias Constructor = (Point -> Color -> Path)


type alias Points = List Point


type Path
  = Pencil Points Color
  | Line Point Point Color
  | Circle Point Float Color
  | Rectangle Point Point Color
  | Null


pencil : Constructor
pencil point color =
  Pencil [point] color


line : Constructor
line point color =
  Line point point color


circle : Constructor
circle point color =
  Circle point 0.0 color


rectangle : Constructor
rectangle point color =
  Rectangle point point color


null = Null


update : Point -> Path -> Path
update point path =
  case path of
    Pencil points color ->
      Pencil (points ++ [point]) color
    Line origin _ color ->
      Line origin point color
    Circle origin _ color ->
      let
        (x1, y1) = origin
        (x2, y2) = point
        radius = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
      in
        Circle origin radius color
    Rectangle origin _ color ->
      Rectangle origin point color
    Null ->
      Null


toCollageForm : Path -> Collage.Form
toCollageForm path =
  case path of
    Pencil points color ->
      let
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        (Collage.traced lineStyle (Collage.path points))
    Line origin point color ->
      let
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        (Collage.traced lineStyle (Collage.path [origin, point]))
    Circle point radius color ->
      let
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        Collage.circle radius
          |> (Collage.outlined lineStyle)
          |> (Collage.move point)
    Rectangle origin point color ->
      let
        (x1, y1) = origin
        (x2, y2) = point
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        Collage.rect ((x2 - x1) * 2) ((y2 - y1) * 2)
          |> (Collage.outlined lineStyle)
          |> (Collage.move origin)
    Null ->
      Collage.toForm Element.empty
