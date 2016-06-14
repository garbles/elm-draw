module Canvas exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , setCurrentColor
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


-- MODEL


type Path = PencilPath (List Point) Color | CirclePath (List Point) Color


type alias Model =
  { isDragging : Bool
  , paths : List Path
  , currentColor : Color
  , size : Window.Size
  }


type alias Flags =
  { defaultColor : Color
  , defaultSize : Window.Size
  }


init : Flags -> (Model, Cmd Msg)
init flags = (Model False [] flags.defaultColor flags.defaultSize, Cmd.none)


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
        path = headPathList model.paths
        nextPath = updatePath model position path
        paths = [nextPath] ++ (tailPathList model.paths)
      in
        ({ model | paths = paths }, Cmd.none)
    DragEnd position ->
      ({ model | isDragging = False }, Cmd.none)


newPath : Model -> Mouse.Position -> Path
newPath model position =
  PencilPath [toPoint model.size position] model.currentColor


updatePath : Model -> Mouse.Position -> Path -> Path
updatePath model position path =
  case path of
    PencilPath points color ->
      PencilPath (points ++ [toPoint model.size position]) color
    CirclePath points color ->
      path


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
    CirclePath points color ->
      let
        defaultLine = Collage.defaultLine
        lineStyle = { defaultLine | color = color, width = 3 }
      in
        (Collage.traced lineStyle (Collage.path points))



-- UTILS


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.Decode.map DragStart Mouse.position)


setCurrentColor : Model -> Color -> Model
setCurrentColor model color = { model | currentColor = color }


setSize : Model -> Window.Size -> Model
setSize model size = { model | size = size }


toPoint : Window.Size -> Mouse.Position -> Point
toPoint size pos =
  (toFloat (pos.x - size.width // 2), toFloat (size.height // 2 - pos.y))


headPathList : List Path -> Path
headPathList list =
  case List.head list of
    Just result ->
      result
    Nothing ->
      PencilPath [] black


tailPathList : List Path -> List Path
tailPathList list =
  case List.tail list of
    Just result ->
      result
    Nothing ->
      []
