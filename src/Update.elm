module Update exposing (Msg, update, subscriptions)

import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Model exposing (model, Model, GameObject)
import Key exposing (fromCode)


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate time ->
            ( applyPhysics model time, Cmd.none )

        KeyDown keyCode ->
            ( keyDown keyCode model, Cmd.none )

        KeyUp keyCode ->
            ( keyUp keyCode model, Cmd.none )


keyDown : KeyCode -> Model -> Model
keyDown keyCode model =
    case fromCode keyCode of
        Key.R ->
            Model.model

        Key.ArrowLeft ->
            { model | paddle = updatePaddleVelocity model.paddle -5.0 }

        Key.ArrowRight ->
            { model | paddle = updatePaddleVelocity model.paddle 5.0 }

        _ ->
            model


keyUp : KeyCode -> Model -> Model
keyUp keyCode model =
    case fromCode keyCode of
        Key.ArrowLeft ->
            { model | paddle = updatePaddleVelocity model.paddle 0 }

        Key.ArrowRight ->
            { model | paddle = updatePaddleVelocity model.paddle 0 }

        _ ->
            model


updatePaddleVelocity : GameObject -> Float -> GameObject
updatePaddleVelocity paddle vx =
    { paddle | vx = vx }


applyPhysics : Model -> Float -> Model
applyPhysics model time =
    -- TODO: Eventally should be this:
    -- updatePositions >> updateHitboxes >> calculateCollisions >> updateFromCollisions model time
    updatePositions model time


updatePositions : Model -> Float -> Model
updatePositions model time =
    let
        ball =
            updateBallPosition model.ball time

        paddle =
            updatePaddlePosition model.paddle time
    in
        { model
            | ball = ball
            , paddle = paddle
        }


updatePaddlePosition : GameObject -> Time -> GameObject
updatePaddlePosition paddle time =
    { paddle | x = updatePosition 0 850 time paddle.x paddle.vx }


updateBallPosition : GameObject -> Time -> GameObject
updateBallPosition ball time =
    { ball
        | x = updatePosition 10 990 time ball.x ball.vx
        , y = updatePosition 10 2000 time ball.y ball.vy
    }


updatePosition : Float -> Float -> Time -> Float -> Float -> Float
updatePosition lowerBound upperBound dt position velocity =
    let
        newPosition =
            position + velocity * dt / 16
    in
        if (newPosition < lowerBound) then
            lowerBound
        else if (newPosition > upperBound) then
            upperBound
        else
            newPosition


updateBallVelocity : GameObject -> Maybe GameObject -> GameObject
updateBallVelocity ball collidedObject =
    -- TODO contribute some of collidedObject's velocity to ball
    ball


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
