module Game exposing (main)

import Html exposing (Html, div, Attribute )
import View exposing (view)

import Model exposing (model, Model)
import Update exposing (Msg, update, subscriptions)

init : ( Model, Cmd Msg )
init = ( model, Cmd.none )

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }