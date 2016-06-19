module ColorPicker exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , getCurrentColor
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (Color)


import Utils.Color exposing (hexToColor, colorToHex)


-- MODEL


type alias Model =
  { color : Color
  }


type alias Flags =
  { color : Color
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  (Model flags.color, Cmd.none)


-- UPDATE


type Msg = SetColor Color


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetColor color ->
      ({ model | color = color }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW


inputStyle = style [
    ("border", "none"),
    ("width", "20px"),
    ("background", "transparent"),
    ("padding", "0"),
    ("margin", "0"),
    ("outline", "none")
  ]


view : Model -> Html Msg
view model =
  div [] [viewColor model.color]


viewColor : Color -> Html Msg
viewColor color =
  input [ type' "color", inputStyle, value (colorToHex color), onChangeColor ] []


-- UTILS


onChangeColor : Attribute Msg
onChangeColor = onInput (hexToColor >> SetColor)


getCurrentColor : Model -> Color
getCurrentColor = .color
