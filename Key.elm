module Key exposing (..)

type Key
    = ArrowLeft
    | ArrowRight
    | R
    | Unknown

fromCode : Int -> Key
fromCode keyCode =
    case keyCode of
        37 ->
            ArrowLeft
        39 ->
            ArrowRight
        82 ->
            R
        _ ->
            Unknown
