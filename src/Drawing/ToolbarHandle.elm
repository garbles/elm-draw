module ToolbarHandle exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , getPosition
  , isOpen
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (Color)
import Json.Decode as Decode
import Mouse

import Point exposing (Point)


-- MODEL


type alias Model =
  { isDragging : Bool
  , isOpen : Bool
  , position : Point
  , startOffset : Point
  }


type alias Flags = {}


init : Flags -> (Model, Cmd Msg)
init flags =
  (Model False True (10, 10) (0, 0), Cmd.none)


-- UPDATE


type Msg =
  DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position
  | ToggleOpen


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
    ToggleOpen ->
      ({ model | isOpen = not model.isOpen }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  if model.isDragging then
    Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
  else
    Sub.none


-- VIEW

wrapperStyle = style [
    ("position", "relative"),
    ("width", "100%"),
    ("height", "20px"),
    ("background-color", "#ccc")
  ]

toggleStyle = style [
    ("position", "absolute"),
    ("right", "0"),
    ("top", "0"),
    ("width", "20px"),
    ("height", "20px")
  ]


view : Model -> Html Msg
view model =
  div [] [
    div [ wrapperStyle, onMouseDown ] [],
    div [ toggleStyle, onClick ToggleOpen ] [ text (if model.isOpen then "X" else "O") ]
  ]


-- UTILS


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Decode.map DragStart Mouse.position)


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


isOpen : Model -> Bool
isOpen = .isOpen
