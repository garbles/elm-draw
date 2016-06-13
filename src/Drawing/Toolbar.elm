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
import ToolbarHandle


-- MODEL


type alias Model =
  { colorPicker : ColorPicker.Model
  , handle : ToolbarHandle.Model
  }


type alias Flags =
  { defaultColors : List Color
  , defaultColor : Color
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  let
    (colorPicker, cmd) = ColorPicker.init flags
    (handle, cmd2) = ToolbarHandle.init {}
  in
    (
      Model colorPicker handle,
      Cmd.batch [
        Cmd.map UpdateColorPicker cmd,
        Cmd.map UpdateToolbarHandle cmd2
      ]
    )


-- UPDATE


type Msg =
  UpdateColorPicker ColorPicker.Msg
  | UpdateToolbarHandle ToolbarHandle.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateColorPicker msg ->
      let
        (colorPicker, cmd) = (ColorPicker.update msg model.colorPicker)
      in
        ({ model | colorPicker = colorPicker }, Cmd.map UpdateColorPicker cmd)
    UpdateToolbarHandle msg ->
      let
        (handle, cmd) = (ToolbarHandle.update msg model.handle)
      in
        ({ model | handle = handle }, Cmd.map UpdateToolbarHandle cmd)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Sub.map UpdateColorPicker (ColorPicker.subscriptions model.colorPicker),
    Sub.map UpdateToolbarHandle (ToolbarHandle.subscriptions model.handle)
  ]

-- VIEW


view : Model -> Html Msg
view model =
  let
    (left, top) = ToolbarHandle.getPosition model.handle
    styles = [
      ("position", "absolute"),
      ("top", (toString top) ++ "px"),
      ("left", (toString left) ++ "px"),
      ("width", "400px")
    ]
  in
    div [ style styles ] [
      App.map UpdateToolbarHandle (ToolbarHandle.view model.handle),
      viewBody model
    ]


viewBody : Model -> Html Msg
viewBody model =
  if (ToolbarHandle.isOpen model.handle) then
    App.map UpdateColorPicker (ColorPicker.view model.colorPicker)
  else
    div [] []


-- UTILS


getCurrentColor : Model -> Color
getCurrentColor model =
  ColorPicker.getCurrentColor model.colorPicker
