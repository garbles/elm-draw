module ToolPicker exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , getCurrentTool
  )

import Html exposing (..)
import Html.Events exposing (..)

import Tool exposing (Tool)


-- MODEL


type alias Model =
  { currentTool : Tool
  , tools: List Tool
  }

type alias Flags =
  { tool : Tool
  , tools : List Tool
  }

init : Flags -> (Model, Cmd Msg)
init flags = (Model flags.tool flags.tools, Cmd.none)


-- UPDATE


type Msg = SetTool Tool


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetTool tool ->
      ({ model | currentTool = tool }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div [] (List.map viewTool model.tools)

viewTool : Tool -> Html Msg
viewTool tool =
  button [ onClick (SetTool tool) ] [ text (toString tool) ]


-- UTILS


getCurrentTool : Model -> Tool
getCurrentTool = .currentTool
