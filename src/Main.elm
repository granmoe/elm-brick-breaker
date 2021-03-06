module Main exposing (main)

import Html exposing (program)
import View exposing (view)
import Model exposing (model, Model)
import Update exposing (Msg, generateBrickColors, update, subscriptions)


init : ( Model, Cmd Msg )
init =
    ( model, generateBrickColors model )


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
