-- TODO: pencil doesn't handle click without drag
-- TODO: move scaling out of styledRect
-- TODO: ignore right clicks
-- TODO: handle going off canvas
-- TODO: scale rounding errors


module Main exposing (..)

import Bitwise exposing (and, shiftRightBy)
import Browser
import Html exposing (Html, button, div, input, label)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Svg exposing (Svg, path, rect, svg, text)
import Svg.Attributes exposing (d, fill, height, stroke, strokeLinecap, strokeLinejoin, strokeWidth, viewBox, width, x, y)


type alias Position =
    { x : Int, y : Int }


type Tool
    = Pencil
    | Square


type alias Model =
    { shapes : List (Svg Msg)
    , preview : List (Svg Msg)
    , scale : Int
    , width : Int
    , height : Int
    , strokeWidth : Int
    , strokeColor : String
    , strokeAlpha : Int
    , fillColor : String
    , fillAlpha : Int
    , dragStart : Position
    , dragContinue : List Position
    , tool : Tool
    }


type Msg
    = EndShape Position
    | StartShape Position
    | ContinueShape Position
    | ZoomIn
    | ZoomOut
    | StrokeWidth String
    | StrokeColor String
    | StrokeAlpha String
    | FillColor String
    | FillAlpha String
    | SetTool Tool


toHexDigit : Int -> String
toHexDigit v =
    if v < 10 then
        String.fromInt v

    else
        case v of
            10 ->
                "a"

            11 ->
                "b"

            12 ->
                "c"

            13 ->
                "d"

            14 ->
                "e"

            15 ->
                "f"

            _ ->
                "0"


toHex : Int -> String
toHex v =
    toHexDigit (and 15 (shiftRightBy 4 v)) ++ toHexDigit (and 15 v)


topLeft : Position -> Position -> Position
topLeft a b =
    { x = min a.x b.x, y = min a.y b.y }


bottomRight : Position -> Position -> Position
bottomRight a b =
    { x = max a.x b.x, y = max a.y b.y }


styledRect : Position -> Position -> Model -> Svg Msg
styledRect start end model =
    let
        tl =
            topLeft start end

        br =
            bottomRight start end
    in
    rect
        [ x (String.fromInt (tl.x // model.scale))
        , y (String.fromInt (tl.y // model.scale))
        , width (String.fromFloat (toFloat (br.x - tl.x) / toFloat model.scale))
        , height (String.fromFloat (toFloat (br.y - tl.y) / toFloat model.scale))
        , stroke (model.strokeColor ++ toHex model.strokeAlpha)
        , fill (model.fillColor ++ toHex model.fillAlpha)
        , strokeWidth (String.fromInt model.strokeWidth)
        ]
        []


pathPosition : String -> Position -> String
pathPosition prefix pos =
    prefix ++ String.fromInt pos.x ++ " " ++ String.fromInt pos.y


scaledPosition : Position -> Model -> Position
scaledPosition pos model =
    { x = pos.x // model.scale, y = pos.y // model.scale }


styledPath : Model -> List Position -> Svg Msg
styledPath model next =
    path
        [ d (String.join " " (List.append [ pathPosition "M" model.dragStart ] (List.map (pathPosition "L") (List.append model.dragContinue next))))
        , stroke (model.strokeColor ++ toHex model.strokeAlpha)
        , strokeWidth (String.fromInt model.strokeWidth)
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , fill "#00000000"
        ]
        []


pencilStart : Model -> Position -> Model
pencilStart model pos =
    { model
        | dragStart = scaledPosition pos model
        , preview = [ styledPath model [] ]
    }


pencilContinue : Model -> Position -> Model
pencilContinue model pos =
    case model.preview of
        [] ->
            model

        _ ->
            let
                next =
                    [ scaledPosition pos model ]
            in
            { model
                | dragContinue = List.append model.dragContinue next
                , preview = [ styledPath model next ]
            }


pencilEnd : Model -> Position -> Model
pencilEnd model pos =
    { model
        | shapes =
            List.append model.shapes
                [ styledPath model [] ]
        , preview = []
        , dragContinue = []
    }


squareStart : Model -> Position -> Model
squareStart model pos =
    { model
        | dragStart = pos
        , preview =
            [ styledRect pos pos model ]
    }


squareContinue : Model -> Position -> Model
squareContinue model pos =
    case model.preview of
        [] ->
            model

        _ ->
            { model
                | preview =
                    [ styledRect model.dragStart pos model ]
            }


squareEnd : Model -> Position -> Model
squareEnd model pos =
    { model
        | shapes =
            List.append model.shapes
                [ styledRect model.dragStart pos model ]
        , preview = []
    }


toolStart : Model -> Position -> Model
toolStart model =
    case model.tool of
        Pencil ->
            pencilStart model

        Square ->
            squareStart model


toolContinue : Model -> Position -> Model
toolContinue model =
    case model.tool of
        Pencil ->
            pencilContinue model

        Square ->
            squareContinue model


toolEnd : Model -> Position -> Model
toolEnd model =
    case model.tool of
        Pencil ->
            pencilEnd model

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
    , strokeColor = "#000000"
    , strokeAlpha = 255
    , fillColor = "#ffffff"
    , fillAlpha = 255
    , dragStart = { x = 0, y = 0 }
    , dragContinue = []
    , tool = Pencil
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

        StrokeWidth width ->
            { model
                | strokeWidth =
                    case String.toInt width of
                        Just i ->
                            i

                        Nothing ->
                            0
            }

        StrokeColor color ->
            { model
                | strokeColor = color
            }

        StrokeAlpha alpha ->
            { model
                | strokeAlpha =
                    case String.toInt alpha of
                        Just i ->
                            i

                        Nothing ->
                            0
            }

        FillColor color ->
            { model
                | fillColor = color
            }

        FillAlpha alpha ->
            { model
                | fillAlpha =
                    case String.toInt alpha of
                        Just i ->
                            i

                        Nothing ->
                            0
            }

        SetTool tool ->
            case tool of
                Pencil ->
                    { model | tool = Pencil }

                Square ->
                    { model | tool = Square }


view : Model -> Html Msg
view model =
    div [ class "application" ]
        [ div [ class "toolbar" ]
            [ div [ class "tool-group" ]
                [ button [ onClick ZoomOut ] [ text "Zoom Out" ]
                , button [ onClick ZoomIn ] [ text "Zoom In" ]
                ]
            , div [ class "tool-group" ]
                [ label []
                    [ text "Width:"
                    , input
                        [ type_ "range"
                        , Html.Attributes.min "1"
                        , Html.Attributes.max "100"
                        , value (String.fromInt model.strokeWidth)
                        , onInput StrokeWidth
                        ]
                        []
                    , text (String.fromInt model.strokeWidth)
                    ]
                ]
            , div [ class "tool-group" ]
                [ label [] [ text "Stroke:", input [ type_ "Color", value model.strokeColor, onInput StrokeColor ] [] ]
                , label []
                    [ text "Opacity:"
                    , input
                        [ type_ "range"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , value (String.fromInt model.strokeAlpha)
                        , onInput StrokeAlpha
                        ]
                        []
                    , text (String.fromInt (model.strokeAlpha * 100 // 255))
                    ]
                ]
            , div [ class "tool-group" ]
                [ label [] [ text "Fill:", input [ type_ "Color", value model.fillColor, onInput FillColor ] [] ]
                , label []
                    [ text "Opacity:"
                    , input
                        [ type_ "range"
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , value (String.fromInt model.fillAlpha)
                        , onInput FillAlpha
                        ]
                        []
                    , text (String.fromInt (model.fillAlpha * 100 // 255))
                    ]
                ]
            ]
        , div [ class "toolbar" ]
            [ div [ class "tool-group" ]
                [ label []
                    [ input
                        [ type_ "radio"
                        , name "tool"
                        , checked (model.tool == Pencil)
                        , onClick (SetTool Pencil)
                        ]
                        []
                    , text "Pencil"
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
