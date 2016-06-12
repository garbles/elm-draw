module ToolbarHandle exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , getPosition
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (Color)
import Json.Decode
import Mouse

import Point exposing (Point)


-- MODEL


type alias Model =
  { isDragging : Bool
  , position : Point
  , startOffset : Point
  }


type alias Flags = {}


init : Flags -> (Model, Cmd Msg)
init flags =
  (Model False (10, 10) (0, 0), Cmd.none)


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
        startOffset = deltaPoints (toPoint position) model.position
      in
        ({ model | isDragging = True, startOffset = startOffset }, Cmd.none)
    DragAt position ->
      ({ model | position = deltaPoints (toPoint position) model.startOffset }, Cmd.none)
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
    styles = [
      ("width", "100%"),
      ("height", "20px"),
      ("background-color", "#ccc")
    ]
  in
    div [ style styles, onMouseDown ] []


-- UTILS


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.Decode.map DragStart Mouse.position)


toPoint : Mouse.Position -> Point
toPoint pos = (toFloat pos.x, toFloat pos.y)


deltaPoints : Point -> Point -> Point
deltaPoints topLeft click =
  let
    (x1, y1) = topLeft
    (x2, y2) = click
  in
    (x1 - x2, y1 - y2)


getPosition : Model -> Point
getPosition = .position
