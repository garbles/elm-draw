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


-- MODEL


type alias Model =
  { color : Color,
    colors : List Color
  }


type alias Flags =
  { defaultColor : Color
  , defaultColors : List Color
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  (Model flags.defaultColor flags.defaultColors, Cmd.none)


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


view : Model -> Html Msg
view model =
  div [] (List.map (viewColor model.color) model.colors)


viewColor : Color -> Color -> Html Msg
viewColor currentColor color =
  let
    styles = [
      ("display", "inline-block"),
      ("background-color", (toRgbString color)),
      ("width", "50px"),
      ("height", "50px"),
      ("box-shadow", if currentColor == color then "inset 0px 0px 0px 3px rgba(255,255,255,0.5)" else "")
    ]
  in
    div [ style styles, onClick (SetColor color) ] []


-- UTILS


toRgbString : Color -> String
toRgbString color =
  let
      rgb = Color.toRgb color
  in
     "rgb(" ++ toString rgb.red ++ "," ++ toString rgb.green ++ "," ++ toString rgb.blue ++ ")"


getCurrentColor : Model -> Color
getCurrentColor = .color
