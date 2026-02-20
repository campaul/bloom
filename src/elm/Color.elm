module Color exposing (..)


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
