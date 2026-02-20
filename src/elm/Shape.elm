module Shape exposing (..)

import Svg exposing (Svg, path, rect)
import Svg.Attributes
    exposing
        ( d
        , fill
        , height
        , stroke
        , strokeLinecap
        , strokeLinejoin
        , strokeWidth
        , width
        , x
        , y
        )


type alias RectProps =
    { name : String
    , x : Float
    , y : Float
    , width : Float
    , height : Float
    , stroke : String
    , fill : String
    , strokeWidth : Int
    }


type alias PathProps =
    { name : String
    , d : String
    , stroke : String
    , strokeWidth : Int
    , strokeLinecap : String
    , strokeLinejoin : String
    , fill : String
    }


type Shape
    = Rect RectProps
    | Path PathProps


name : Shape -> String
name shape =
    case shape of
        Rect props ->
            props.name

        Path props ->
            props.name


toSvg : Shape -> Svg msg
toSvg shape =
    case shape of
        Rect props ->
            rect
                [ x (String.fromFloat props.x)
                , y (String.fromFloat props.y)
                , width (String.fromFloat props.width)
                , height (String.fromFloat props.height)
                , stroke props.stroke
                , fill props.fill
                , strokeWidth (String.fromInt props.strokeWidth)
                ]
                []

        Path props ->
            path
                [ d props.d
                , stroke props.stroke
                , strokeWidth (String.fromInt props.strokeWidth)
                , strokeLinecap props.strokeLinecap
                , strokeLinejoin props.strokeLinejoin
                , fill props.fill
                ]
                []
