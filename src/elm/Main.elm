-- TODO: pencil doesn't handle click without drag
-- TODO: move scaling out of styledRect


module Main exposing (..)

import Browser
import Html exposing (Html, div, img, input, label, li, p, ul)
import Html.Attributes exposing (checked, class, name, src, step, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Shape exposing (Shape)
import Svg
    exposing
        ( Svg
        , circle
        , defs
        , linearGradient
        , rect
        , stop
        , svg
        , text
        )
import Svg.Attributes
    exposing
        ( cx
        , cy
        , fill
        , gradientTransform
        , height
        , id
        , offset
        , r
        , rx
        , ry
        , stopColor
        , stroke
        , strokeWidth
        , style
        , viewBox
        , width
        , x
        , y
        )


type alias Position =
    { x : Float, y : Float, button : Int }


type Tool
    = Pencil
    | Square


type alias Color =
    { hue : Int
    , saturation : Float
    , value : Float
    , alpha : Float
    , selecting : Bool
    }


rgbaToHex : Float -> Float -> Float -> Float -> String
rgbaToHex r g b a =
    "rgba(" ++ String.fromInt (round (r * 255)) ++ "," ++ String.fromInt (round (g * 255)) ++ "," ++ String.fromInt (round (b * 255)) ++ "," ++ String.fromFloat a ++ ")"


rgba : Color -> String
rgba color =
    let
        h =
            toFloat color.hue / 360

        s =
            color.saturation

        v =
            color.value

        i =
            floor (h * 6)

        f =
            h * 6 - toFloat i

        p =
            v * (1 - s)

        q =
            v * (1 - f * s)

        t =
            v * (1 - (1 - f) * s)
    in
    case remainderBy 6 i of
        0 ->
            rgbaToHex v t p color.alpha

        1 ->
            rgbaToHex q v p color.alpha

        2 ->
            rgbaToHex p v t color.alpha

        3 ->
            rgbaToHex p q v color.alpha

        4 ->
            rgbaToHex t p v color.alpha

        5 ->
            rgbaToHex v p q color.alpha

        _ ->
            ""


type alias Model =
    { shapes : List Shape
    , preview : List Shape
    , scale : Float
    , width : Int
    , height : Int
    , strokeWidth : Int
    , strokeColor : Color
    , fillColor : Color
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
    | ChangeStrokeColor Color
    | ChangeFillColor Color
    | SetTool Tool


topLeft : Position -> Position -> Position
topLeft a b =
    { a | x = min a.x b.x, y = min a.y b.y }


bottomRight : Position -> Position -> Position
bottomRight a b =
    { a | x = max a.x b.x, y = max a.y b.y }


changeHue : Color -> (Color -> Msg) -> String -> Msg
changeHue color msg val =
    case String.toInt val of
        Just i ->
            msg { color | hue = i }

        Nothing ->
            msg { color | hue = 0 }


changeOpacity : Color -> (Color -> Msg) -> String -> Msg
changeOpacity color msg val =
    case String.toInt val of
        Just i ->
            msg { color | alpha = toFloat i / 100 }

        Nothing ->
            msg { color | alpha = 1 }


changeSatVal : Color -> (Color -> Msg) -> Position -> Msg
changeSatVal color msg pos =
    if pos.button == 0 then
        msg
            { color
                | saturation = max 0 (min 1 (pos.x / 255))
                , value = max 0 (min 1 ((255 - pos.y) / 255))
                , selecting = True
            }

    else
        msg color


maybeChangeSatVal : Color -> (Color -> Msg) -> Position -> Msg
maybeChangeSatVal color msg pos =
    if color.selecting then
        msg
            { color
                | saturation = max 0 (min 1 (pos.x / 255))
                , value = max 0 (min 1 ((255 - pos.y) / 255))
            }

    else
        msg color


endChangeSatVal : Color -> (Color -> Msg) -> Position -> Msg
endChangeSatVal color msg pos =
    msg
        { color
            | saturation = max 0 (min 1 (pos.x / 255))
            , value = max 0 (min 1 ((255 - pos.y) / 255))
            , selecting = False
        }


colorPicker : Color -> (Color -> Msg) -> Svg Msg
colorPicker color msg =
    div
        [ class "color-picker" ]
        [ svg
            [ width "256"
            , height "256"
            , viewBox "0 0 256 256"
            , on "mousedown" (Decode.map (changeSatVal color msg) decodePosition)
            , on "mousemove" (Decode.map (maybeChangeSatVal color msg) decodePosition)
            , on "mouseup" (Decode.map (endChangeSatVal color msg) decodePosition)
            ]
            [ defs
                []
                [ linearGradient
                    [ id "g1" ]
                    [ stop
                        [ offset "0", stopColor "white" ]
                        []
                    , stop
                        [ offset "1", stopColor "black" ]
                        []
                    ]
                , linearGradient
                    [ id "g2", gradientTransform "rotate(90)" ]
                    [ stop
                        [ offset "0", stopColor "white" ]
                        []
                    , stop
                        [ offset "1", stopColor "black" ]
                        []
                    ]
                ]
            , rect
                [ x "0", y "0", rx "4", ry "4", width "256", height "256", fill ("hsl(" ++ String.fromInt color.hue ++ ", 100%, 50%)") ]
                []
            , rect
                [ x "0", y "0", rx "4", ry "4", width "256", height "256", fill "url('#g1')", style "mix-blend-mode:screen" ]
                []
            , rect
                [ x "0", y "0", rx "4", ry "4", width "256", height "256", fill "url('#g2')", style "mix-blend-mode:multiply" ]
                []
            , circle
                [ cx (String.fromFloat (color.saturation * 255))
                , cy (String.fromFloat (255 - color.value * 255))
                , r "8"
                , fill (rgba { color | alpha = 1 })
                , stroke "#fff"
                , strokeWidth "2"
                ]
                []
            ]
        , input
            [ type_ "range"
            , Html.Attributes.min "0"
            , Html.Attributes.max "360"
            , value (String.fromInt color.hue)
            , class "hue-picker"
            , style ("--selected-color:" ++ rgba { color | saturation = 1, value = 1, alpha = 1 })
            , onInput (changeHue color msg)
            ]
            []
        , input
            [ type_ "range"
            , step "1"
            , value (String.fromFloat (color.alpha * 100))
            , class "opacity-picker"
            , style ("--selected-hue:" ++ rgba { color | alpha = 1 } ++ ";--selected-color:" ++ rgba color)
            , onInput (changeOpacity color msg)
            ]
            []
        , div
            [ class "swatch", style ("--selected-color:" ++ rgba color) ]
            []
        ]


styledRect : Position -> Position -> Model -> Shape
styledRect start end model =
    let
        tl =
            topLeft start end

        br =
            bottomRight start end
    in
    Shape.Rect
        { name = "Rect"
        , x = tl.x / model.scale
        , y = tl.y / model.scale
        , width = (br.x - tl.x) / model.scale
        , height = (br.y - tl.y) / model.scale
        , stroke = rgba model.strokeColor
        , fill = rgba model.fillColor
        , strokeWidth = model.strokeWidth
        }


pathPosition : String -> Position -> String
pathPosition prefix pos =
    prefix ++ String.fromFloat pos.x ++ " " ++ String.fromFloat pos.y


scaledPosition : Position -> Model -> Position
scaledPosition pos model =
    { pos | x = pos.x / model.scale, y = pos.y / model.scale }


styledPath : Model -> List Position -> Shape
styledPath model next =
    Shape.Path
        { name = "Path"
        , d = String.join " " (List.append [ pathPosition "M" model.dragStart ] (List.map (pathPosition "L") (List.append model.dragContinue next)))
        , stroke = rgba model.strokeColor
        , strokeWidth = model.strokeWidth
        , strokeLinecap = "round"
        , strokeLinejoin = "round"
        , fill = "#00000000"
        }


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
pencilEnd model _ =
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
    case model.preview of
        [] ->
            model

        _ ->
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
    Decode.map3 Position
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        (Decode.field "button" Decode.int)


init : Model
init =
    { shapes = []
    , preview = []
    , scale = 1
    , width = 800
    , height = 600
    , strokeWidth = 3
    , strokeColor =
        { hue = 0
        , saturation = 1
        , value = 0
        , alpha = 1
        , selecting = False
        }
    , fillColor =
        { hue = 0
        , saturation = 1
        , value = 1
        , alpha = 0
        , selecting = False
        }
    , dragStart = { x = 0, y = 0, button = 0 }
    , dragContinue = []
    , tool = Pencil
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartShape pos ->
            if pos.button == 0 then
                toolStart model pos

            else
                model

        ContinueShape pos ->
            toolContinue model pos

        EndShape pos ->
            if pos.button == 0 then
                toolEnd model pos

            else
                model

        ZoomIn ->
            { model | scale = min (model.scale * 2) 16 }

        ZoomOut ->
            { model | scale = max (model.scale / 2) 1 }

        StrokeWidth width ->
            { model
                | strokeWidth =
                    case String.toInt width of
                        Just i ->
                            i

                        Nothing ->
                            0
            }

        ChangeStrokeColor color ->
            { model | strokeColor = color }

        ChangeFillColor color ->
            { model | fillColor = color }

        SetTool tool ->
            case tool of
                Pencil ->
                    { model | tool = Pencil }

                Square ->
                    { model | tool = Square }


layerView : Shape -> Html Msg
layerView shape =
    li
        []
        [ text (Shape.name shape) ]


view : Model -> Html Msg
view model =
    div [ class "application" ]
        [ div [ class "toolbars" ]
            [ div [ class "toolbar" ]
                [ div [ class "tool-group" ]
                    [ img [ onClick ZoomOut, src "assets/icons/zoom-out.svg", class "icon-button" ] []
                    , img [ onClick ZoomIn, src "assets/icons/zoom-in.svg", class "icon-button" ] []
                    ]
                , div [ class "tool-group" ]
                    [ label []
                        [ text "Stroke Width:"
                        , input
                            [ type_ "range"
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , value (String.fromInt model.strokeWidth)
                            , onInput StrokeWidth
                            ]
                            []
                        , text (String.fromInt model.strokeWidth ++ "px")
                        ]
                    ]
                ]
            ]
        , div [ class "sidebar tools" ]
            [ p [] [ text "Tools" ]
            , div [ class "tool-group" ]
                [ label []
                    [ input
                        [ type_ "radio"
                        , name "tool"
                        , checked (model.tool == Pencil)
                        , onClick (SetTool Pencil)
                        ]
                        []
                    , img [ src "assets/icons/pencil.svg", class "icon-button" ] []
                    ]
                , label []
                    [ input
                        [ type_ "radio"
                        , name "tool"
                        , checked (model.tool == Square)
                        , onClick (SetTool Square)
                        ]
                        []
                    , img [ src "assets/icons/square.svg", class "icon-button" ] []
                    ]
                ]
            ]
        , div [ class "viewport" ]
            [ svg
                [ width (String.fromInt (model.width * floor model.scale))
                , height (String.fromInt (model.height * floor model.scale))
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
                (List.map Shape.toSvg (List.append model.shapes model.preview))
            ]
        , div
            [ class "sidebar" ]
            [ p [] [ text "Stroke Color" ]
            , colorPicker model.strokeColor ChangeStrokeColor
            , p [] [ text "Fill Color" ]
            , colorPicker model.fillColor ChangeFillColor
            , p [] [ text "Layers" ]
            , ul
                []
                (List.map layerView model.shapes)
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
