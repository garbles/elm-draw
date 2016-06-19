module Toolbar exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , getCurrentColor
  , getCurrentTool
  )


import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Color exposing (Color, black)

import ColorPicker
import ToolbarHandle
import ToolPicker
import Tool exposing (Tool(..))


-- MODEL


type alias Model =
  { colorPicker : ColorPicker.Model
  , handle : ToolbarHandle.Model
  , toolPicker : ToolPicker.Model
  }


type alias Flags =
  { defaultColor : Color
  , defaultTools : List Tool
  , defaultTool : Tool
  }


init : Flags -> (Model, Cmd Msg)
init flags =
  let
    { defaultColor, defaultTool, defaultTools } = flags
    (colorPicker, cmd) = ColorPicker.init { defaultColor = defaultColor }
    (handle, cmd2) = ToolbarHandle.init {}
    (toolPicker, cmd3) = ToolPicker.init { defaultTool = defaultTool, defaultTools = defaultTools }
  in
    (
      Model colorPicker handle toolPicker,
      Cmd.batch [
        Cmd.map UpdateColorPicker cmd,
        Cmd.map UpdateToolbarHandle cmd2,
        Cmd.map UpdateToolPicker cmd3
      ]
    )


-- UPDATE


type Msg =
  UpdateColorPicker ColorPicker.Msg
  | UpdateToolbarHandle ToolbarHandle.Msg
  | UpdateToolPicker ToolPicker.Msg

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
    UpdateToolPicker msg ->
      let
        (toolPicker, cmd) = (ToolPicker.update msg model.toolPicker)
      in
        ({ model | toolPicker = toolPicker }, Cmd.map UpdateToolPicker cmd)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Sub.map UpdateColorPicker (ColorPicker.subscriptions model.colorPicker),
    Sub.map UpdateToolbarHandle (ToolbarHandle.subscriptions model.handle),
    Sub.map UpdateToolPicker (ToolPicker.subscriptions model.toolPicker)
  ]


-- VIEW


view : Model -> Html Msg
view model =
  let
    (left, top) = ToolbarHandle.getPosition model.handle
    wrapperStyle = style [
      ("position", "absolute"),
      ("top", (toString top) ++ "px"),
      ("left", (toString left) ++ "px"),
      ("width", "400px")
    ]
  in
    div [ wrapperStyle ]
      ([App.map UpdateToolbarHandle (ToolbarHandle.view model.handle)] ++ (viewBody model))


viewBody : Model -> List (Html Msg)
viewBody model =
  if (ToolbarHandle.isOpen model.handle) then
    [
      App.map UpdateColorPicker (ColorPicker.view model.colorPicker),
      App.map UpdateToolPicker (ToolPicker.view model.toolPicker)
    ]
  else
    [
      div [] []
    ]


-- UTILS


getCurrentColor : Model -> Color
getCurrentColor model =
  ColorPicker.getCurrentColor model.colorPicker

getCurrentTool : Model -> Tool
getCurrentTool model =
  ToolPicker.getCurrentTool model.toolPicker
