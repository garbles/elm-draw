module Drawing exposing (main)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Color exposing (Color)
import Task
import Window
import Random

import Canvas
import Toolbar
import Tool exposing (Tool)


defaultSize = { width = 1000, height = 500 }
defaultColor = Color.black
defaultFill = Color.rgba 0 0 0 0
defaultTool = Tool.pencil
defaultTools = Tool.list


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  {
    canvas : Canvas.Model,
    toolbar : Toolbar.Model
  }


init : (Model, Cmd Msg)
init =
  let
    (canvas, cmd) = Canvas.init
      { size = defaultSize
      , color = defaultColor
      , fill = defaultFill
      , tool = defaultTool
      }

    (toolbar, cmd2) = Toolbar.init
      { color = defaultColor
      , fill = defaultFill
      , tools = defaultTools
      , tool = defaultTool
      }
  in
    (
      Model canvas toolbar,
      Cmd.batch [
        (Task.perform identity GetWindowSize Window.size),
        Cmd.map UpdateCanvas cmd,
        Cmd.map UpdateToolbar cmd2
      ]
    )


-- UPDATE


type Msg =
  UpdateCanvas Canvas.Msg
  | UpdateToolbar Toolbar.Msg
  | GetWindowSize Window.Size


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateCanvas msg ->
      let
        (canvas, cmd) = Canvas.update msg model.canvas
      in
        ({ model | canvas = canvas }, Cmd.map UpdateCanvas cmd)
    UpdateToolbar msg ->
      let
        (toolbar, cmd) = Toolbar.update msg model.toolbar
        canvas = model.canvas
          |> Canvas.setCurrentColor (Toolbar.getCurrentColor toolbar)
          |> Canvas.setCurrentFill (Toolbar.getCurrentFill toolbar)
          |> Canvas.setCurrentTool (Toolbar.getCurrentTool toolbar)
      in
        ({ model | toolbar = toolbar, canvas = canvas }, Cmd.map UpdateToolbar cmd)
    GetWindowSize size ->
      let
        canvas = Canvas.setSize model.canvas size
      in
        ({ model | canvas = canvas }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [
    Window.resizes GetWindowSize,
    Sub.map UpdateCanvas (Canvas.subscriptions model.canvas),
    Sub.map UpdateToolbar (Toolbar.subscriptions model.toolbar)
  ]


-- VIEW


wrapperStyle = style [
    ("cursor", "pointer"),
    ("user-select", "none")
  ]


view : Model -> Html Msg
view model =
  div [ wrapperStyle ]
  [
    (App.map UpdateCanvas (Canvas.view model.canvas)),
    (App.map UpdateToolbar (Toolbar.view model.toolbar))
  ]
