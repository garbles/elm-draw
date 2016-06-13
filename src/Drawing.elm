import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (style)
import Color exposing (Color)
import Task
import Window

import Canvas
import Toolbar

defaultColors = [
    Color.black,
    Color.red,
    Color.orange,
    Color.yellow,
    Color.green,
    Color.blue,
    Color.purple,
    Color.brown
  ]
defaultSize = { width = 1000, height = 500 }
defaultColor = Color.black

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
    (canvas, cmd) = Canvas.init { defaultSize = defaultSize, defaultColor = defaultColor }
    (toolbar, cmd2) = Toolbar.init { defaultColors = defaultColors, defaultColor = defaultColor }
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
        canvas = Canvas.setCurrentColor model.canvas (Toolbar.getCurrentColor toolbar)
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


view : Model -> Html Msg
view model =
  div [ style [("cursor", "pointer")] ]
  [
    (App.map UpdateCanvas (Canvas.view model.canvas)),
    (App.map UpdateToolbar (Toolbar.view model.toolbar))
  ]
