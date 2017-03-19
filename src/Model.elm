module Model exposing (Model, model, generateBricks, GameObject, GameObjectName(Brick, Paddle, Ball), CollisionData, collisionData)

import Rectangle exposing (Rectangle)
import Config exposing (config, GameProgress)


type alias CollisionData =
    { offsetX : Float
    , offsetY : Float
    , vx : Float
    , vy : Float
    , hasCollision : Bool
    , bricks : List GameObject
    }


collisionData : CollisionData
collisionData =
    { offsetX = 0
    , offsetY = 0
    , vx = 0
    , vy = 0
    , hasCollision = False
    , bricks = []
    }


type alias Model =
    { width : Float
    , height : Float
    , paddle : GameObject
    , ball : GameObject
    , bricks : List GameObject
    , brickCount : Int
    , remainingLives : Int
    , gameProgress : GameProgress
    }


type GameObjectName
    = Brick
    | Paddle
    | Ball


type alias GameObject =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , width : Float
    , height : Float
    , hitbox : Rectangle
    , objectType : GameObjectName
    , health : Float
    , color : String
    }


generateBricks : List GameObject -> List String -> Int -> Float -> Float -> List GameObject
generateBricks bricks colors total x y =
    let
        getNextX x =
            if (x >= config.gameWidth - config.brickWidth) then
                0
            else
                x + config.brickWidth
    in
        case total of
            0 ->
                bricks

            _ ->
                generateBricks
                    ((generateBrick (List.head colors) x y)
                        :: bricks
                    )
                    (List.drop 1 colors)
                    (total - 1)
                    (getNextX x)
                    (if (x >= config.gameWidth - config.brickWidth) then
                        y + config.brickHeight
                     else
                        y
                    )


generateBrick : Maybe String -> Float -> Float -> GameObject
generateBrick color x y =
    let
        color_ =
            case color of
                Just val ->
                    val

                Nothing ->
                    config.defaultBrickColor
    in
        { brickTemplate
            | color = color_
            , x = x
            , y = y
            , hitbox =
                { xs = ( x, x + config.brickWidth )
                , ys = ( y, y + config.brickHeight )
                }
        }


brickTemplate : GameObject
brickTemplate =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , width = config.brickWidth
    , height = config.brickHeight
    , hitbox =
        { xs = ( 0, 0 )
        , ys = ( 0, 0 )
        }
    , objectType = Brick
    , health = config.brickHealth
    , color = ""
    }


paddleInitX : Float
paddleInitX =
    (config.gameWidth / 2) - (config.paddleWidth / 2)


ballInitX : Float
ballInitX =
    (config.gameWidth / 2) - (config.ballDiameter / 2)


model : Model
model =
    { width = config.gameWidth
    , height = config.gameHeight
    , paddle =
        { x = paddleInitX
        , y = config.paddleInitY
        , vx = 0
        , vy = 0
        , width = config.paddleWidth
        , height = config.paddleHeight
        , hitbox =
            { xs = ( paddleInitX, paddleInitX + config.paddleWidth )
            , ys = ( config.paddleInitY, config.paddleInitY + config.paddleHeight )
            }
        , objectType = Paddle
        , health = config.paddleHealth
        , color = config.paddleColor
        }
    , ball =
        { x = ballInitX
        , y = config.ballInitY
        , vx = 0
        , vy = config.ballInitVY
        , width = config.ballDiameter
        , height = config.ballDiameter
        , hitbox =
            { xs = ( ballInitX, ballInitX + config.ballDiameter )
            , ys = ( config.ballInitY, config.ballInitY + config.ballDiameter )
            }
        , objectType = Ball
        , health = config.ballHealth
        , color = config.ballColor
        }
    , bricks = []
    , remainingLives = config.remainingLives
    , brickCount = config.brickCount
    , gameProgress = config.gameProgress
    }
