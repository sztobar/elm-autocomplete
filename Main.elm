port module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Array exposing (Array)

-- STYLES

labelStyle : List (String, String)
labelStyle =
  [ ("font-family", "inconsolata")
  , ("width", "100px")
  , ("display", "inline-block")
  , ("text-align", "right")
  ]

inputStyle : List (String, String)
inputStyle =
  [ ( "width", "460px" )
  , ( "padding", "5px" )
  , ( "font-size", "15px" )
  , ( "font-family", "inconsolata" )
  , ( "height", "30px" )
  ]

ulStyle : Model -> List (String, String)
ulStyle model =
  [ ( "position", "absolute" )
  , ( "left", "0px" )
  , ( "right", "0px" )
  , ( "top", "25px" )
  , ( "margin", "0" )
  , ( "padding", "0" )
  , ( "list-style", "none" )
  , ( "background-color", "white" )
  , ( "font-family", "inconsolata" )
  , ( "border-top", "1px solid #ccc" )
  , ( "border-left", "1px solid #ccc" )
  , ( "border-right", "1px solid #ccc" )
  , ( "box-sizing", "border-box" )
  , ( "box-shadow", "0px 4px 4px rgb(220, 220, 220)" )
  , ( "z-index", "999" )
  , ( "display", ulDisplay model )
  ]

ulDisplay : Model -> String
ulDisplay {focused, items} =
  if focused && not (Array.isEmpty items) then
    "block"
  else
    "none"

liStyle : (Int, Model) -> List (String, String)
liStyle (i, {highlighted}) =
  [ ( "cursor", "pointer" )
  , ( "list-style", "none" )
  , ( "padding", "3px 0 3px 8px" )
  , ( "margin", "0" )
  , ( "border-bottom", "1px solid #ccc" )
  , ( "background-color", liBackgroundColor (i, highlighted) )
  ]

liBackgroundColor : (Int, Maybe Int) -> String
liBackgroundColor (i, highlighted) =
  case highlighted of

    Just i' ->
      if i == i' then
        "#ccccff"
      else
        "inherit"

    _ ->
      "inherit"

enterKey : Int
enterKey = 13

upArrowKey : Int
upArrowKey = 38

downArrowKey : Int
downArrowKey = 40

tabKey : Int
tabKey = 9

escKey : Int
escKey = 27

-- DECODERS
  
keyCodeDecoder : Int -> Result String HighlightMsg
keyCodeDecoder kc =
  if kc == downArrowKey then
     Result.Ok Next

  else if kc == upArrowKey then
     Result.Ok Previous

  else if kc == enterKey then
     Result.Ok SelectHighlighted

  else if kc == escKey then
     Result.Ok Clear

  else
     Result.Err ""

keyMsg : Json.Decoder HighlightMsg
keyMsg =
  Json.customDecoder keyCode keyCodeDecoder 

tabKeyMsg : Json.Decoder HighlightMsg
tabKeyMsg =
  Json.customDecoder keyCode (\kc ->
    if kc == tabKey then
      Result.Ok SelectHighlighted
    else
      keyCodeDecoder kc
  )

-- EVENTS

onQueryKeyDown : (HighlightMsg -> msg) -> Bool -> Html.Attribute msg
onQueryKeyDown tagger prevent =
  if prevent then
    onWithOptions "keydown" preventDefault (Json.map tagger tabKeyMsg)
  else
    on "keydown" (Json.map tagger keyMsg)

preventDefault : Html.Events.Options
preventDefault =
  { stopPropagation = False
  , preventDefault = True
  }

onMouseDown : msg -> Html.Attribute msg
onMouseDown msg =
  onWithOptions "mousedown" preventDefault (Json.succeed msg)

onMouseUp : msg -> Html.Attribute msg
onMouseUp msg =
  onWithOptions "mouseup" preventDefault (Json.succeed msg)

-- VIEWS

view model =
  div []
    [ div []
      [ label [ style labelStyle ] [ text "Query:" ]
      , span [ style [ ("position", "relative") ] ]
        [ input [ style inputStyle
                , type' "text"
                , value model.value
                , onInput Input
                , onQueryKeyDown KeyDown (model.focused && (maybeToBoolean model.highlighted))
                , onBlur Blur
                , onFocus Focus
                , onClick Focus ] []
        , ul [ style (ulStyle model) ]
          ((Array.indexedMap (\i v ->
            li [ style (liStyle (i, model))
               , onMouseDown (MouseDown i)
               , onMouseUp (MouseUp i)
               , onMouseOver (Highlight i)
               , onMouseOut UnHighlight ]
               [ text v ]
            )
          model.items)
            |> Array.toList)
        ]
      ]
    , div []
      [ label [ style labelStyle ] [ text "Some Field:" ]
      , span [ style [ ("position", "relative") ] ]
        [ input [ style inputStyle
                , type' "text"] []
        ]
      ]
    ]

type alias Model =
  { value : String
  , items : Array String
  , highlighted : Maybe Int
  , focused: Bool
  , mouseDown : Maybe Int
  }

-- ACTION

type Msg
  = Focus
  | Blur
  | Input String
  | Highlight Int
  | UnHighlight
  | MouseUp Int
  | MouseDown Int
  | Response (Array String)
  | KeyDown HighlightMsg

type HighlightMsg
  = Next
  | Previous
  | Clear
  | SelectHighlighted

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Focus ->
      (focusUpdate model, Cmd.none)

    Blur ->
      (blurUpdate model, Cmd.none)

    Input val ->
      (inputUpdate model val, makeQuery val)

    Response items ->
      (responseUpdate model items, Cmd.none)

    Highlight i->
      (highlightUpdate model i, Cmd.none)

    UnHighlight ->
      (unhighlightUpdate model, Cmd.none)

    MouseDown i ->
      (mouseDownUpdate model i, Cmd.none)

    MouseUp i ->
      (mouseUpUpdate model i, Cmd.none)

    KeyDown hm ->
      (keyDownUpdate model hm, Cmd.none)

focusUpdate : Model -> Model
focusUpdate model =
  { model | focused = True }

blurUpdate : Model -> Model
blurUpdate model =
  { model | focused = False }

inputUpdate : Model -> String -> Model
inputUpdate model val =
  { model
   | value = val
   , focused = False
   , highlighted = Nothing
  }
 
responseUpdate : Model -> Array String -> Model
responseUpdate model items =
  { model
   | items = items
   , focused = True
  }

highlightUpdate : Model -> Int -> Model
highlightUpdate model i =
  { model | highlighted = Just i }

unhighlightUpdate : Model -> Model
unhighlightUpdate model =
  { model | highlighted = Nothing }

mouseDownUpdate : Model -> Int -> Model
mouseDownUpdate model i =
  { model | mouseDown = Just i }

mouseUpUpdate : Model -> Int -> Model
mouseUpUpdate model i =
  case model.mouseDown of
    Just j ->
      if i == j then
        { model
         | focused = False
         , mouseDown = Nothing
         , value = Maybe.withDefault "" (Array.get i model.items)
        }
      else
        { model | mouseDown = Nothing }
    Nothing ->
      model

keyDownUpdate : Model -> HighlightMsg -> Model
keyDownUpdate model msg =
  let
    len = Array.length model.items
  in
    case (model.highlighted, msg) of
      (Nothing, Next) ->
        { model | highlighted = Just 0 }

      (Nothing, Previous) ->
        { model | highlighted = Just (len - 1) }

      (Just i, Next) ->
        { model | highlighted = Just ((i + 1) % len) }

      (Just i, Previous) ->
        { model | highlighted = Just ((i - 1) % len) }

      (_, Clear) ->
        blurUpdate model

      (Just i, SelectHighlighted) ->
        { model
         | focused = False
         , value = Maybe.withDefault "" (Array.get i model.items)
        }
      _ ->
        model
 
-- UTILS

maybeToBoolean : Maybe a -> Bool
maybeToBoolean v =
  case v of
    Just a ->
      True

    Nothing ->
      False

-- SUBSCRIPTIONS

baseUrl : String
baseUrl = "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search="

makeQuery : String -> Cmd msg
makeQuery query =
  let
    url = baseUrl ++ query ++ "&callback=elm_autocomplete"
  in
    request url

subscriptions : Model -> Sub Msg
subscriptions model =
  response Response

-- PORTS

port request : String -> Cmd msg

port response : (Array String -> msg) -> Sub msg

-- MAIN

init : (Model, Cmd Msg)
init =
  ({ value = ""
  , items = Array.fromList []
  , focused = False
  , highlighted = Nothing
  , mouseDown = Nothing
  }, Cmd.none)

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
