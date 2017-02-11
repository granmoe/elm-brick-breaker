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
    -- or maybe use a monad
    updatePositions model time |> updateHitboxes |> handleWallCollisions



-- |> handleObjectCollisions


updatePositions : Model -> Float -> Model
updatePositions model time =
    let
        ball =
            updateBallPosition model.ball time

        paddle =
            clampPosition (updatePaddlePosition model.paddle time) (model.width - model.paddle.width) model.height
    in
        { model
            | ball = ball
            , paddle = paddle
        }


updatePaddlePosition : GameObject -> Time -> GameObject
updatePaddlePosition paddle time =
    { paddle | x = updatePosition time paddle.x paddle.vx }


updateBallPosition : GameObject -> Time -> GameObject
updateBallPosition ball time =
    { ball
        | x = updatePosition time ball.x ball.vx
        , y = updatePosition time ball.y ball.vy
    }


updatePosition : Time -> Float -> Float -> Float
updatePosition dt position velocity =
    position + velocity * dt / 16


clampPosition : GameObject -> Float -> Float -> GameObject
clampPosition gameObject maxX maxY =
    -- assuming mins of 0
    let
        x =
            if (gameObject.x < 0) then
                0
            else if (gameObject.x > maxX) then
                maxX
            else
                gameObject.x

        y =
            if (gameObject.y < 0) then
                0
            else if (gameObject.y > maxY) then
                maxY
            else
                gameObject.y
    in
        { gameObject
            | x = x
            , y = y
        }


updateHitboxes : Model -> Model
updateHitboxes model =
    let
        paddle =
            updateHitbox model.paddle

        ball =
            updateHitbox model.ball
    in
        { model
            | paddle = paddle
            , ball = ball
        }


updateHitbox : GameObject -> GameObject
updateHitbox gameObject =
    { gameObject
        | hitbox = { xs = ( gameObject.x, gameObject.x + gameObject.width ), ys = ( gameObject.y, gameObject.y + gameObject.height ) }
    }


handleWallCollisions : Model -> Model
handleWallCollisions model =
    let
        ball =
            collideBallWithWalls model.ball model.width model.height
    in
        { model
            | ball = ball
        }


collideBallWithWalls : GameObject -> Float -> Float -> GameObject
collideBallWithWalls ball maxX maxY =
    -- assuming mins of 0
    let
        vx =
            if (ball.x < 0 || ball.x > maxX) then
                -ball.vx
            else
                ball.vx

        vy =
            if (ball.y < 0 || ball.y > maxY) then
                -ball.vy
            else
                ball.vy
    in
        { ball
            | vx = vx
            , vy = vy
        }



-- getCollidedObject : Model -> { model : Model, collidedObject : Maybe GameObject }
-- getCollidedObject model =
--     let
--         collidedObject =
--             collideObjects model.ball [ model.paddle ]
--     in
--         { model = model, collidedObject = collidedObject }
-- collideObjects : GameObject -> List GameObject -> Maybe List GameObject
-- collideObjects object objectsList =
--     Just objectsList


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
