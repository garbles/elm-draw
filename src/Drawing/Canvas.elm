module Canvas exposing
  ( Model
  , Msg
  , init
  , update
  , subscriptions
  , view
  , setCurrentColor
  , setSize
  )


import Html exposing (..)
import Html.Events exposing (on)
import Element exposing (toHtml)
import Collage
import Color exposing (Color, black)
import Json.Decode
import Mouse
import Window


import Point exposing (Point)


-- MODEL


type alias PageSize =
  { width : Int
  , height : Int
  }


type alias Path =
  { points : List Point
  , color : Color
  }


type alias Model =
  { isDragging : Bool
  , paths : List Path
  , currentColor : Color
  , size : PageSize
  }


type alias Flags =
  { defaultColor : Color
  , defaultSize : PageSize
  }


init : Flags -> (Model, Cmd Msg)
init flags = (Model False [] flags.defaultColor flags.defaultSize, Cmd.none)


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
        newPath = Path [toPoint model.size position] model.currentColor
        paths = [newPath] ++ model.paths
      in
        ({ model | paths = paths, isDragging = True }, Cmd.none)
    DragAt position ->
      let
        path = headPathList model.paths
        nextPath = { path | points = (path.points ++ [toPoint model.size position]) }
        paths = [nextPath] ++ (tailPathList model.paths)
      in
        ({ model | paths = paths }, Cmd.none)
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
    { height, width } = model.size
  in
    div [ onMouseDown ]
      [
        toHtml (Collage.collage width height (viewPaths model))
      ]


viewPaths : Model -> List Collage.Form
viewPaths model =
  (List.map viewPath (List.reverse model.paths))


viewPath : Path -> Collage.Form
viewPath path =
  let
    defaultLine = Collage.defaultLine
    lineStyle = { defaultLine | color = path.color, width = 3 }
  in
    (Collage.traced lineStyle (Collage.path path.points))


-- UTILS


onMouseDown : Attribute Msg
onMouseDown =
    on "mousedown" (Json.Decode.map DragStart Mouse.position)


toPoint : PageSize -> Mouse.Position -> Point
toPoint size pos =
  (toFloat (pos.x - size.width // 2), toFloat (size.height // 2 - pos.y))


headPathList : List Path -> Path
headPathList list =
  case List.head list of
    Just result ->
      result
    Nothing ->
      Path [] black


tailPathList : List Path -> List Path
tailPathList list =
  case List.tail list of
    Just result ->
      result
    Nothing ->
      []


setCurrentColor : Model -> Color -> Model
setCurrentColor model color = { model | currentColor = color }


setSize : Model -> Window.Size -> Model
setSize model size = { model | size = size }
