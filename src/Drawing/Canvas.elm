module Canvas exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , setCurrentColor
  , setCurrentTool
  , setSize
  )


import Html exposing (..)
import Html.Events exposing (on)
import Element exposing (toHtml)
import Collage
import Color exposing (Color, black)
import Json.Decode
import Mouse
import Window

import Point exposing (Point)
import Tool exposing (Tool(..))


-- MODEL


type Path = PencilPath (List Point) Color | CirclePath Point Float Color | LinePath Point Point Color | RectanglePath Point Point Color


type alias Model =
  { isDragging : Bool
  , paths : List Path
  , currentTool : Tool
  , currentColor : Color
  , size : Window.Size
  }


type alias Flags =
  { defaultColor : Color
  , defaultSize : Window.Size
  , defaultTool : Tool
  }


init : Flags -> (Model, Cmd Msg)
init flags = (Model False [] flags.defaultTool flags.defaultColor flags.defaultSize, Cmd.none)


-- UPDATE


type Msg =
  DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragStart position ->
      let
        paths = [newPath model position] ++ model.paths
      in
        ({ model | paths = paths, isDragging = True }, Cmd.none)
    DragAt position ->
      let
        defaultPath = PencilPath [] black
        path = headList defaultPath model.paths
        nextPath = updatePath model position path
        paths = [nextPath] ++ (tailList model.paths)
      in
        ({ model | paths = paths }, Cmd.none)
    DragEnd position ->
      ({ model | isDragging = False }, Cmd.none)


newPath : Model -> Mouse.Position -> Path
newPath model position =
  case model.currentTool of
    Pencil ->
      PencilPath [toPoint model.size position] model.currentColor
    Line ->
      let
        point = (toPoint model.size position)
      in
        LinePath point point model.currentColor
    Circle ->
      let
        point = (toPoint model.size position)
      in
        CirclePath point 0.0 model.currentColor
    Rectangle ->
      let
        point = (toPoint model.size position)
      in
        RectanglePath point point model.currentColor


updatePath : Model -> Mouse.Position -> Path -> Path
updatePath model position path =
  case path of
    PencilPath points color ->
      PencilPath (points ++ [toPoint model.size position]) color
    LinePath origin _ color ->
      let
        point = (toPoint model.size position)
      in
        LinePath origin point color
    CirclePath point _ color ->
      let
        positionPoint = toPoint model.size position
        radius = toDistance point positionPoint
      in
        CirclePath point radius color
    RectanglePath origin _ color ->
      let
        point = (toPoint model.size position)
      in
        RectanglePath origin point color

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.isDragging then
    Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
  else
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  let
    { height, width } = model.size
  in
    div [ onMouseDown ]
      [
        toHtml (Collage.collage width height (viewPaths model))
      ]


viewPaths : Model -> List Collage.Form
viewPaths model =
  (List.map viewPath (List.reverse model.paths))


viewPath : Path -> Collage.Form
viewPath path =
  case path of
    PencilPath points color ->
      let
        defaultLine = Collage.defaultLine
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        (Collage.traced lineStyle (Collage.path points))
    LinePath origin point color ->
      let
        defaultLine = Collage.defaultLine
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        (Collage.traced lineStyle (Collage.path [origin, point]))
    CirclePath point radius color ->
      let
        defaultLine = Collage.defaultLine
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        Collage.circle radius
          |> (Collage.outlined lineStyle)
          |> (Collage.move point)
    RectanglePath origin point color ->
      let
        (x1, y1) = origin
        (x2, y2) = point
        defaultLine = Collage.defaultLine
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        Collage.rect ((x2 - x1) * 2) ((y2 - y1) * 2)
          |> (Collage.outlined lineStyle)
          |> (Collage.move origin)


-- UTILS


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.Decode.map DragStart Mouse.position)


setCurrentColor : Color -> Model -> Model
setCurrentColor color model = { model | currentColor = color }


setCurrentTool : Tool -> Model -> Model
setCurrentTool tool model = { model | currentTool = tool }


setSize : Model -> Window.Size -> Model
setSize model size = { model | size = size }


toPoint : Window.Size -> Mouse.Position -> Point
toPoint size pos =
  (toFloat (pos.x - size.width // 2), toFloat (size.height // 2 - pos.y))


toDistance : Point -> Point -> Float
toDistance p1 p2 =
  let
    (x1, y1) = p1
    (x2, y2) = p2
  in
    sqrt <| (x1 - x2)^2 + (y1 - y2)^2


headList : a -> List a -> a
headList default list =
  case List.head list of
    Just result ->
      result
    Nothing ->
      default


tailList : List a -> List a
tailList list =
  case List.tail list of
    Just result ->
      result
    Nothing ->
      []
