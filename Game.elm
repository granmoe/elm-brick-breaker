module Game exposing (..)

import Html exposing (Html, div, Attribute )
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Key exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL
type alias Model =
    { velocity : Float
    , position : Float
    , shotsFired : Int
    }

model : Model
model =
    { velocity = 0
    , position = 0
    , shotsFired = 0
    }

init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


-- UPDATE
type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate dt ->
            ( applyPhysics dt model, Cmd.none )
        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )
        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )

keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case Key.fromCode keyCode of
        R ->
            Tuple.first init
        ArrowLeft ->
            updateVelocity -1.0 model
        ArrowRight ->
            updateVelocity 1.0 model
        _ ->
            model

keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            updateVelocity 0 model
        ArrowRight ->
            updateVelocity 0 model
        _ ->
            model

applyPhysics : Float -> Model -> Model
applyPhysics dt model =
    { model | position = model.position + model.velocity * dt }

updateVelocity : Float -> Model -> Model
updateVelocity newVelocity model =
    { model | velocity = newVelocity }


-- VIEW
wrapperStyle : Html.Attribute msg
wrapperStyle =
    Html.Attributes.style
    [ ("backgroundColor", "papayaWhip")
    , ("position", "fixed")
    , ("top", "0")
    , ("right", "0")
    , ("bottom", "0")
    , ("left", "0")
    , ("padding", "10%")
    ]

view : Model -> Html msg
view model =
    div [ wrapperStyle ]
        [
            svg
                [ width "100%", height "100%", viewBox "0 0 1000 1000" ]
                [ 
                    rect
                    [ fill "black", x "0", y "0", width "1000", height "1000" ]
                    []
                ]
        ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
