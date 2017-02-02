module Game exposing (main)

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
    { paddle : Paddle
    , ball : Ball
    , bricks : List GameObject
    , lostGame : Bool
    , wonGame : Bool
    }

type alias GameObject = 
    {
        x : Float,
        y : Float
    }

type alias Movable a = 
    {
        a |
        velocityX : Float, 
        velocityY : Float
    }

type alias Paddle = Movable(GameObject)
type alias Ball = Movable(GameObject)

-- TODO
bricks : List GameObject
bricks = [
        { x = 0, y = 0 }
    ]

model : Model
model =
    { paddle = {
            x = 425,
            y = 940,
            velocityX = 0,
            velocityY = 0
        }
    , ball = {
            x = 450,
            y = 900,
            velocityX = 0,
            velocityY = -5
        }
    , bricks = bricks
    , lostGame = False
    , wonGame = False
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
            { model | paddle = updatePaddleVelocity model.paddle -5.0 }
        ArrowRight ->
            { model | paddle = updatePaddleVelocity model.paddle 5.0 }
        _ ->
            model

keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case Key.fromCode keyCode of
        ArrowLeft ->
            { model | paddle = updatePaddleVelocity model.paddle 0 }
        ArrowRight ->
            { model | paddle = updatePaddleVelocity model.paddle 0 }
        _ ->
            model

applyPhysics : Float -> Model -> Model
applyPhysics dt model = { model | paddle = updatePaddlePosition dt model.paddle }

updatePaddlePosition : Time -> Movable(GameObject) -> Paddle
updatePaddlePosition dt paddle = 
    let
        newX = paddle.x + paddle.velocityX * dt / 16
    in
        if (newX < 0) then
            { paddle | x = 0 } 
        else if (newX > 850) then 
            { paddle | x = 850 } 
        else
            { paddle | x = newX }

updatePaddleVelocity : Paddle -> Float -> Paddle
updatePaddleVelocity paddle velocityX = { paddle | velocityX = velocityX }

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
                    , paddle model
                ]
        ]

paddle : Model -> Html msg
paddle model =
    rect
    [ fill "white", x <| toString model.paddle.x, y <| toString model.paddle.y, width "150", height "25", rx "5", ry "5" ]
    []

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
