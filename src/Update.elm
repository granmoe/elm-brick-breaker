module Update exposing (Msg, update, subscriptions, generateBrickColors)

import Random
import Hex
import AnimationFrame
import Keyboard exposing (KeyCode)
import Time exposing (Time)
import Model exposing (model, Model, GameObject, generateBricks, collisionData, CollisionData, GameObjectName(Brick, Paddle, Ball))
import Key exposing (fromCode)
import Rectangle exposing (rectangleIntersection)
import Config exposing (GameProgress(..))


type Msg
    = TimeUpdate Time
    | KeyDown KeyCode
    | KeyUp KeyCode
    | BrickColors (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    if (model.gameProgress == Lost) then
        waitForTryAgain msg model
    else
        let
            newModel =
                generateBricks msg <| handleInput msg <| applyPhysics msg model
        in
            ( newModel, Cmd.none )



-- TODO: Logic for decrementing remainingLives on restart, going to next level depending on gameProgress, starting the game over if won game or lost game


waitForTryAgain : Msg -> Model -> ( Model, Cmd Msg )
waitForTryAgain msg model =
    case msg of
        KeyDown keyCode ->
            ( Model.model, generateBrickColors Model.model )

        _ ->
            ( model, Cmd.none )


generateBrickColors : Model -> Cmd Msg
generateBrickColors model =
    Random.generate BrickColors <| Random.list model.brickCount <| (Random.int 1000000 16777215)


generateBricks : Msg -> Model -> Model
generateBricks msg model =
    let
        createBricks model colors =
            { model | bricks = Model.generateBricks [] colors 20 0 100 }
    in
        case msg of
            BrickColors colors ->
                createBricks model <| List.map (\n -> "#" ++ Hex.toString n) colors

            _ ->
                model


handleInput : Msg -> Model -> Model
handleInput msg model =
    case msg of
        KeyDown keyCode ->
            keyDown keyCode model

        KeyUp keyCode ->
            keyUp keyCode model

        _ ->
            model


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


applyPhysics : Msg -> Model -> Model
applyPhysics msg model =
    case msg of
        TimeUpdate time ->
            updatePositions model time |> updateHitboxes |> loseGame |> handleWallCollisions |> handleObjectCollisions |> clampPositions

        _ ->
            model


updatePositions : Model -> Float -> Model
updatePositions model time =
    let
        ball =
            updateObjectPosition model.ball time

        paddle =
            updateObjectPosition model.paddle time
    in
        { model
            | ball = ball
            , paddle = paddle
        }


updateObjectPosition : GameObject -> Time -> GameObject
updateObjectPosition object time =
    let
        updateAxisPosition dt position velocity =
            position + velocity * dt / 16
    in
        { object
            | x = updateAxisPosition time object.x object.vx
            , y = updateAxisPosition time object.y object.vy
        }


updateHitboxes : Model -> Model
updateHitboxes model =
    let
        updateHitbox gameObject =
            { gameObject
                | hitbox = { xs = ( gameObject.x, gameObject.x + gameObject.width ), ys = ( gameObject.y, gameObject.y + gameObject.height ) }
            }

        paddle =
            updateHitbox model.paddle

        ball =
            updateHitbox model.ball
    in
        { model
            | paddle = paddle
            , ball = ball
        }


loseGame : Model -> Model
loseGame model =
    if model.ball.y > (model.height - model.ball.height) then
        { model
            | gameProgress = Lost
        }
    else
        -- check if no bricks, set to next level
        model


handleWallCollisions : Model -> Model
handleWallCollisions model =
    let
        ball =
            collideBallWithWalls model.ball (model.width - model.ball.width) (model.height - model.ball.height)
    in
        { model
            | ball = ball
        }


collideBallWithWalls : GameObject -> Float -> Float -> GameObject
collideBallWithWalls ball maxX maxY =
    let
        changeVelocity pos max v =
            if (pos <= 0 || pos >= max) then
                -v
            else
                v

        vx =
            changeVelocity ball.x maxX ball.vx

        vy =
            changeVelocity ball.y maxY ball.vy
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

        addObjectVelocity intersects v objV =
            if (intersects) then
                v + objV / 3
            else
                v

        damageBrick intersects health =
            if (intersects) then
                health - 50
            else
                health
    in
        { data
            | offsetX = data.offsetX + offsetX
            , offsetY = data.offsetY + offsetY
            , vx = addObjectVelocity intersects data.vx object.vx
            , vy = addObjectVelocity intersects data.vy object.vy
            , hasCollision = data.hasCollision || intersects
            , bricks =
                case object.objectType of
                    Brick ->
                        { object | health = damageBrick intersects object.health } :: data.bricks

                    _ ->
                        data.bricks
        }


calculateTranslationVector : GameObject -> GameObject -> { offsetX : Float, offsetY : Float, intersects : Bool }
calculateTranslationVector ball object =
    let
        flipOffset intersects v offset =
            if (intersects && v > 0) then
                -offset
            else
                offset

        { x, y, intersects } =
            rectangleIntersection ball.hitbox object.hitbox
    in
        { offsetX = flipOffset intersects ball.vx x
        , offsetY = flipOffset intersects ball.vy y
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
            , bricks = List.filter (\brick -> brick.health > 0) bricks
        }


updateBallFromCollisions : GameObject -> Bool -> Float -> Float -> Float -> Float -> GameObject
updateBallFromCollisions ball hasCollision offsetX offsetY vx vy =
    let
        calculateOffset offset currentAxisOffset otherAxisOffset =
            if (currentAxisOffset < otherAxisOffset) then
                offset + currentAxisOffset
            else
                offset

        calculateDirection hasCollision v currentAxisOffset otherAxisOffset =
            if (hasCollision && currentAxisOffset < otherAxisOffset) then
                -v
            else
                v
    in
        { ball
            | x = calculateOffset ball.x offsetX offsetY
            , y = calculateOffset ball.y offsetY offsetX
            , vx = calculateDirection hasCollision (ball.vx + vx) (abs offsetX) (abs offsetY)
            , vy = calculateDirection hasCollision (ball.vy + vy) (abs offsetY) (abs offsetX)
        }


clampPositions : Model -> Model
clampPositions model =
    let
        ball =
            clampPosition (model.width - model.ball.width) (model.height - model.ball.height) model.ball

        paddle =
            clampPosition (model.width - model.paddle.width) (model.height - model.paddle.height) model.paddle
    in
        { model
            | ball = ball
            , paddle = paddle
        }


clampPosition : Float -> Float -> GameObject -> GameObject
clampPosition maxX maxY gameObject =
    let
        clamp num min max =
            if (num < min) then
                min
            else if (num > max) then
                max
            else
                num

        x =
            clamp gameObject.x 0 maxX

        y =
            clamp gameObject.y 0 maxY
    in
        { gameObject
            | x = x
            , y = y
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs TimeUpdate
        , Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]
