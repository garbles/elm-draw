module Toolbar exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , getCurrentColor
  )


import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (Color, black)

import ColorPicker
import Point exposing (Point)


-- MODEL


type alias Model =
  {
    colorPicker : ColorPicker.Model,
    position : Point
  }


type alias Flags =
  { defaultColors : List Color
  , defaultColor : Color
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  let
    (colorPicker, cmd) = ColorPicker.init flags
  in
    (Model colorPicker (10,10), Cmd.map UpdateColorPicker cmd)


-- UPDATE


type Msg = UpdateColorPicker ColorPicker.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateColorPicker msg ->
      let
        (colorPicker, cmd) = (ColorPicker.update msg model.colorPicker)
      in
        ({ model | colorPicker = colorPicker }, Cmd.map UpdateColorPicker cmd)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- VIEW


view : Model -> Html Msg
view model =
  let
    (top, left) = model.position
    styles = [
      ("position", "absolute"),
      ("top", (toString top) ++ "px"),
      ("left", (toString left) ++ "px")
    ]
  in
    div [ style styles ] [
      App.map UpdateColorPicker (ColorPicker.view model.colorPicker)
    ]


-- UTILS


getCurrentColor : Model -> Color
getCurrentColor model =
  ColorPicker.getCurrentColor model.colorPicker
