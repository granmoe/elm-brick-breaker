module Update exposing (Msg, update, subscriptions)

import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)

import Model exposing (model, Model, GameObject)
import Key exposing (Key, fromCode)

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
    case fromCode keyCode of
        Key.R ->
            model
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

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
