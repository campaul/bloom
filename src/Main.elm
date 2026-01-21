module Main exposing (..)

import Browser
import Html exposing (Html, button, div, fieldset, input, label, span)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Svg exposing (Svg, circle, rect, svg, text)
import Svg.Attributes exposing (cx, cy, height, r, viewBox, width, x, y)


type alias Position =
    { x : Int, y : Int }


type Tool
    = Brush
    | Square


type alias Model =
    { shapes : List (Svg Msg)
    , preview : List (Svg Msg)
    , scale : Int
    , width : Int
    , height : Int
    , strokeWidth : Int
    , dragStart : Position
    , tool : Tool
    }


type Msg
    = EndShape Position
    | StartShape Position
    | ContinueShape Position
    | ZoomIn
    | ZoomOut
    | StrokeWidth String
    | SetTool Tool


topLeft : Position -> Position -> Position
topLeft a b =
    { x = min a.x b.x, y = min a.y b.y }


bottomRight : Position -> Position -> Position
bottomRight a b =
    { x = max a.x b.x, y = max a.y b.y }


scaledRect : Position -> Position -> Int -> Svg Msg
scaledRect start end scale =
    let
        tl =
            topLeft start end

        br =
            bottomRight start end
    in
    rect
        [ x (String.fromInt (tl.x // scale))
        , y (String.fromInt (tl.y // scale))
        , width (String.fromFloat (toFloat (br.x - tl.x) / toFloat scale))
        , height (String.fromFloat (toFloat (br.y - tl.y) / toFloat scale))
        ]
        []


brushStart : Model -> Position -> Model
brushStart model pos =
    { model
        | dragStart = pos
        , preview =
            [ circle
                [ cx (String.fromInt (pos.x // model.scale))
                , cy (String.fromInt (pos.y // model.scale))
                , r (String.fromInt model.strokeWidth)
                ]
                []
            ]
    }


brushContinue : Model -> Position -> Model
brushContinue model pos =
    case model.preview of
        [] ->
            model

        _ ->
            { model
                | preview =
                    [ circle
                        [ cx (String.fromInt (pos.x // model.scale))
                        , cy (String.fromInt (pos.y // model.scale))
                        , r (String.fromInt model.strokeWidth)
                        ]
                        []
                    ]
            }


brushEnd : Model -> Position -> Model
brushEnd model pos =
    { model
        | shapes =
            List.append model.shapes
                [ circle
                    [ cx (String.fromInt (pos.x // model.scale))
                    , cy (String.fromInt (pos.y // model.scale))
                    , r (String.fromInt model.strokeWidth)
                    ]
                    []
                ]
        , preview = []
    }


squareStart : Model -> Position -> Model
squareStart model pos =
    { model
        | dragStart = pos
        , preview =
            [ scaledRect pos pos model.scale ]
    }


squareContinue : Model -> Position -> Model
squareContinue model pos =
    case model.preview of
        [] ->
            model

        _ ->
            { model
                | preview =
                    [ scaledRect model.dragStart pos model.scale ]
            }


squareEnd : Model -> Position -> Model
squareEnd model pos =
    { model
        | shapes =
            List.append model.shapes
                [ scaledRect model.dragStart pos model.scale ]
        , preview = []
    }


toolStart : Model -> Position -> Model
toolStart model =
    case model.tool of
        Brush ->
            brushStart model

        Square ->
            squareStart model


toolContinue : Model -> Position -> Model
toolContinue model =
    case model.tool of
        Brush ->
            brushContinue model

        Square ->
            squareContinue model


toolEnd : Model -> Position -> Model
toolEnd model =
    case model.tool of
        Brush ->
            brushEnd model

        Square ->
            squareEnd model


decodePosition : Decode.Decoder Position
decodePosition =
    Decode.map2 Position
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


init : Model
init =
    { shapes = []
    , preview = []
    , scale = 1
    , width = 800
    , height = 600
    , strokeWidth = 5
    , dragStart = { x = 0, y = 0 }
    , tool = Brush
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartShape pos ->
            toolStart model pos

        ContinueShape pos ->
            toolContinue model pos

        EndShape pos ->
            toolEnd model pos

        ZoomIn ->
            { model | scale = min (model.scale * 2) 16 }

        ZoomOut ->
            { model | scale = max (model.scale // 2) 1 }

        StrokeWidth strokeWidth ->
            { model
                | strokeWidth =
                    case String.toInt strokeWidth of
                        Just i ->
                            i

                        Nothing ->
                            0
            }

        SetTool tool ->
            case tool of
                Brush ->
                    { model | tool = Brush }

                Square ->
                    { model | tool = Square }


view : Model -> Html Msg
view model =
    div [ class "application" ]
        [ div [ class "toolbar" ]
            [ button [ onClick ZoomOut ] [ text "Zoom Out" ]
            , button [ onClick ZoomIn ] [ text "Zoom In" ]
            , fieldset []
                [ label []
                    [ input
                        [ type_ "radio"
                        , name "tool"
                        , checked (model.tool == Brush)
                        , onClick (SetTool Brush)
                        ]
                        []
                    , text "Brush"
                    ]
                , label []
                    [ input
                        [ type_ "radio"
                        , name "tool"
                        , checked (model.tool == Square)
                        , onClick (SetTool Square)
                        ]
                        []
                    , text "Square"
                    ]
                ]
            , input
                [ type_ "range"
                , Html.Attributes.min "1"
                , Html.Attributes.max "100"
                , value (String.fromInt model.strokeWidth)
                , onInput StrokeWidth
                ]
                []
            , span [] [ text (String.fromInt model.strokeWidth) ]
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
                , on "mousedown" (Decode.map StartShape decodePosition)
                , on "mousemove" (Decode.map ContinueShape decodePosition)
                , on "mouseup" (Decode.map EndShape decodePosition)
                ]
                (List.append model.shapes model.preview)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
