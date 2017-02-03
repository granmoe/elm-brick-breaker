module Game exposing (main)

import Html exposing (Html, div, Attribute )
import Html.Attributes exposing (style)
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Key exposing (..)
import Model exposing (model, Model, GameObject)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : ( Model, Cmd Msg )
init = ( model, Cmd.none )

type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode

-- UPDATE
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
applyPhysics dt model = 
    let
        { ball, bricks } = collideObjects model
    in
        { model
            | paddle = updatePaddlePosition dt model.paddle
            , ball = updateBallPosition dt ball
            , bricks = bricks
            }

updatePosition : Float -> Float -> Time -> Float -> Float -> Float
updatePosition lowerBound upperBound dt position velocity = 
    let
        newPosition = position + velocity * dt / 16
    in
        if (newPosition < lowerBound) then
            lowerBound            
        else if (newPosition > upperBound) then 
            upperBound
        else
            newPosition

updatePaddlePosition : Time -> GameObject -> GameObject
updatePaddlePosition time paddle = { paddle | x = updatePosition 0 850 time paddle.x paddle.vx }

updatePaddleVelocity : GameObject -> Float -> GameObject
updatePaddleVelocity paddle vx = { paddle | vx = vx }

updateBallPosition : Time -> GameObject -> GameObject
updateBallPosition time ball = 
    { ball 
        | x = updatePosition 10 990 time ball.x ball.vx
        , y = updatePosition 10 2000 time ball.y ball.vy 
        }

collideObjects : Model -> Model
collideObjects model =
    let 
        { uncollidedBricks } = checkBrickCollisions model.ball model.bricks
        collidedObject = getCollidedObject model
    in
        { model
            | ball = updateBallVelocity model.ball collidedObject
            , bricks = uncollidedBricks
            }

checkBrickCollisions : GameObject -> List GameObject -> { uncollidedBricks : List GameObject }
checkBrickCollisions ball bricks =
    {
        uncollidedBricks = []
    }

getCollidedObject : Model -> Maybe GameObject
getCollidedObject model = Just model.paddle

updateBallVelocity : GameObject -> Maybe GameObject -> GameObject
updateBallVelocity ball collidedObject = ball

-- VIEW
wrapperStyle : Html.Attribute msg
wrapperStyle =
    Html.Attributes.style
    [ ("backgroundColor", "papayawhip")
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
                    , ball model
                ]
        ]

paddle : Model -> Html msg
paddle model =
    rect
    [ fill "gainsboro", x <| toString model.paddle.x, y <| toString model.paddle.y, width "150", height "25", rx "5", ry "5" ]
    []

ball : Model -> Html msg
ball model =
    circle
    [ fill "white", cx <| toString model.ball.x, cy <| toString model.ball.y, r "10" ]
    []

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
