module Util exposing (..)

import Bitwise exposing (and, shiftRightBy)


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
