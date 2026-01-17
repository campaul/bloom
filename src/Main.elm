module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, span)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Svg exposing (Svg, circle, svg, text)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width)


type alias Position =
    { x : Int, y : Int }


type alias Model =
    { shapes : List (Svg Msg)
    , scale : Int
    , width : Int
    , height : Int
    , brushSize : Int
    }


type Msg
    = Append Position
    | ZoomIn
    | ZoomOut
    | BrushSize String


decodePosition : Decode.Decoder Position
decodePosition =
    Decode.map2 Position
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


init : Model
init =
    { shapes = []
    , scale = 1
    , width = 800
    , height = 600
    , brushSize = 5
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Append pos ->
            { model
                | shapes =
                    List.append model.shapes
                        [ circle
                            [ cx (String.fromInt (pos.x // model.scale))
                            , cy (String.fromInt (pos.y // model.scale))
                            , r (String.fromInt model.brushSize)
                            ]
                            []
                        ]
            }

        ZoomIn ->
            { model | scale = min (model.scale * 2) 16 }

        ZoomOut ->
            { model | scale = max (model.scale // 2) 1 }

        BrushSize brushSize ->
            { model
                | brushSize =
                    case String.toInt brushSize of
                        Just i ->
                            i

                        Nothing ->
                            0
            }


view : Model -> Html Msg
view model =
    div [ class "application" ]
        [ div [ class "toolbar" ]
            [ button [ onClick ZoomOut ] [ text "Zoom Out" ]
            , button [ onClick ZoomIn ] [ text "Zoom In" ]
            , input
                [ type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "100"
                , value (String.fromInt model.brushSize)
                , onInput BrushSize
                ]
                []
            , span [] [ text (String.fromInt model.brushSize) ]
            ]
        , div [ class "viewport" ]
            [ svg
                [ width (String.fromInt (model.width * model.scale))
                , height (String.fromInt (model.height * model.scale))
                , viewBox
                    ("0 0 "
                        ++ String.fromInt model.width
                        ++ " "
                        ++ String.fromInt model.height
                    )
                , on "click" (Decode.map Append decodePosition)
                ]
                model.shapes
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
