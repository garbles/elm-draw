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


type alias Constructor = (Point -> Color -> Color -> Path)


type alias Points = List Point


type Path
  = Pencil Points Color
  | Line Point Point Color
  | Circle Point Float Color Color
  | Rectangle Point Point Color Color
  | Null


pencil : Constructor
pencil point color _ =
  Pencil [point] color


line : Constructor
line point color _ =
  Line point point color


circle : Constructor
circle point color fill =
  Circle point 0.0 color fill


rectangle : Constructor
rectangle point color fill =
  Rectangle point point color fill


null = Null


update : Point -> Path -> Path
update point path =
  case path of
    Pencil points color ->
      Pencil (points ++ [point]) color
    Line origin _ color ->
      Line origin point color
    Circle origin _ color fill ->
      let
        (x1, y1) = origin
        (x2, y2) = point
        radius = sqrt ((x1 - x2)^2 + (y1 - y2)^2)
      in
        Circle origin radius color fill
    Rectangle origin _ color fill ->
      Rectangle origin point color fill
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
    Circle point radius color fill ->
      let
        lineStyle = { defaultLine | color = color, width = 3 }

        outline = Collage.circle radius
          |> (Collage.outlined lineStyle)
          |> (Collage.move point)

        filled = Collage.circle (radius - 1.5)
          |> (Collage.filled fill)
          |> (Collage.move point)
      in
        Collage.group [outline, filled]
    Rectangle origin point color fill ->
      let
        (x1, y1) = origin
        (x2, y2) = point
        lineStyle = { defaultLine | color = color, width = 3 }

        outline = Collage.rect ((x2 - x1) * 2) ((y2 - y1) * 2)
          |> (Collage.outlined lineStyle)
          |> (Collage.move origin)

        filled = Collage.rect ((x2 - x1 - 1.5) * 2) ((y2 - y1 + 1.5) * 2)
          |> (Collage.filled fill)
          |> (Collage.move origin)
      in
        Collage.group [outline, filled]
    Null ->
      Collage.toForm Element.empty
