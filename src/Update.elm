module Update exposing (Msg, update, subscriptions)

import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)
import Model exposing (model, Model, GameObject, collisionData, CollisionData, GameObjectName(Brick, Paddle, Ball))
import Key exposing (fromCode)
import Rectangle exposing (rectangleIntersection)


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
    updatePositions model time |> updateHitboxes |> handleWallCollisions |> handleObjectCollisions |> clampPositions


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


clampPosition : Float -> Float -> GameObject -> GameObject
clampPosition maxX maxY gameObject =
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


clampPositions : Model -> Model
clampPositions model =
    let
        ball =
            model.ball

        -- TODO: This is causing bugs for some reason
        -- clampPosition (model.width - model.ball.width - 1) (model.height - model.ball.height - 1) model.ball
        paddle =
            clampPosition (model.width - model.paddle.width) (model.height - model.paddle.height) model.paddle
    in
        { model
            | ball = ball
            , paddle = paddle
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
    let
        vx =
            if (ball.x < 0 || ball.x >= maxX) then
                -ball.vx
            else
                ball.vx

        vy =
            if (ball.y < 0 || ball.y >= maxY) then
                -ball.vy
            else
                ball.vy
    in
        { ball
            | vx = vx
            , vy = vy
        }


handleObjectCollisions : Model -> Model
handleObjectCollisions model =
    applyCollisionData model <| List.foldl (calculateCollisionData model.ball) collisionData <| model.paddle :: model.bricks


calculateCollisionData : GameObject -> GameObject -> CollisionData -> CollisionData
calculateCollisionData ball object data =
    let
        { offsetX, offsetY, intersects } =
            calculateTranslationVector ball object
    in
        { data
            | offsetX = data.offsetX + offsetX
            , offsetY = data.offsetY + offsetY
            , vx =
                if (intersects) then
                    data.vx + object.vx / 4
                else
                    data.vx
            , vy =
                if (intersects) then
                    data.vy + object.vy / 4
                else
                    data.vy
            , hasCollision = data.hasCollision || intersects
            , bricks =
                case object.objectType of
                    Brick ->
                        object :: data.bricks

                    _ ->
                        data.bricks
        }


calculateTranslationVector : GameObject -> GameObject -> { offsetX : Float, offsetY : Float, intersects : Bool }
calculateTranslationVector ball object =
    let
        { x, y, intersects } =
            rectangleIntersection ball.hitbox object.hitbox
    in
        { offsetX =
            if (intersects && ball.vx > 0) then
                -x
            else
                x
        , offsetY =
            if (intersects && ball.vy > 0) then
                -y
            else
                y
        , intersects = intersects
        }


applyCollisionData : Model -> CollisionData -> Model
applyCollisionData model { hasCollision, offsetX, offsetY, vx, vy, bricks } =
    let
        ball =
            updateBallFromCollisions model.ball hasCollision offsetX offsetY vx vy
    in
        { model
            | ball = ball
            , bricks = bricks
        }


updateBallFromCollisions : GameObject -> Bool -> Float -> Float -> Float -> Float -> GameObject
updateBallFromCollisions ball hasCollision offsetX offsetY vx vy =
    { ball
        | x = ball.x + offsetX
        , y = ball.y + offsetY
        , vx =
            if (hasCollision && abs offsetX > abs offsetY) then
                -(ball.vx + vx)
            else
                ball.vx + vx
        , vy =
            if (hasCollision && abs offsetY > abs offsetX) then
                -(ball.vy + vy)
            else
                ball.vy + vy
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
