module Canvas exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , setColor
  , setFill
  , setTool
  , setSize
  )


import Html exposing (..)
import Html.Events exposing (on)
import Element exposing (toHtml)
import Collage
import Color exposing (Color, black)
import Json.Decode as Decode
import Mouse
import Window

import Path exposing (Path)
import Point exposing (Point)
import Tool exposing (Tool)


-- MODEL


type alias Model =
  { isDragging : Bool
  , paths : List Path
  , tool : Tool
  , color : Color
  , fill : Color
  , size : Window.Size
  }


type alias Flags =
  { color : Color
  , fill : Color
  , size : Window.Size
  , tool : Tool
  }


init : Flags -> (Model, Cmd Msg)
init flags = (Model False [] flags.tool flags.color flags.fill flags.size, Cmd.none)


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
        { color, fill, tool } = model
        point = toPoint model.size position
        path = Tool.newPath tool point color fill
        paths = [path] ++ model.paths
      in
        ({ model | paths = paths, isDragging = True }, Cmd.none)
    DragAt position ->
      let
        point = toPoint model.size position
        path = List.head model.paths |> Maybe.withDefault Path.null |> Path.update point
        tail = List.tail model.paths |> Maybe.withDefault []
      in
        ({ model | paths = [path] ++ tail }, Cmd.none)
    DragEnd position ->
      ({ model | isDragging = False }, Cmd.none)


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
  (List.map Path.toCollageForm (List.reverse model.paths))


-- UTILS


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)


setColor : Color -> Model -> Model
setColor color model = { model | color = color }


setFill : Color -> Model -> Model
setFill color model = { model | fill = color }


setTool : Tool -> Model -> Model
setTool tool model = { model | tool = tool }


setSize : Model -> Window.Size -> Model
setSize model size = { model | size = size }


toPoint : Window.Size -> Mouse.Position -> Point
toPoint size pos =
  (toFloat (pos.x - size.width // 2), toFloat (size.height // 2 - pos.y))
