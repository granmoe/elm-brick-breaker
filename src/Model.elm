module Model exposing (Model, model, GameObject, GameObjectName(Brick, Paddle, Ball), CollisionData, collisionData)

import Rectangle exposing (Rectangle)


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
    , remainingLives : Int
    , wonGame : Bool
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


bricks : List GameObject
bricks =
    generateBricks [] 20 0 100


generateBricks : List GameObject -> Int -> Float -> Float -> List GameObject
generateBricks bricks total x y =
    let
        getNextX x =
            if (x == 800) then
                0
            else
                x + 200
    in
        case total of
            0 ->
                bricks

            _ ->
                generateBricks (generateBrick x y :: bricks)
                    (total - 1)
                    (getNextX x)
                    (if (x == 800) then
                        y + 50
                     else
                        y
                    )


generateBrick : Float -> Float -> GameObject
generateBrick x y =
    { brickTemplate
        | x = x
        , y = y
        , hitbox =
            { xs = ( x, x + 200 )
            , ys = ( y, y + 50 )
            }
    }


brickTemplate : GameObject
brickTemplate =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , width = 200
    , height = 50
    , hitbox =
        { xs = ( 0, 0 )
        , ys = ( 0, 0 )
        }
    , objectType = Brick
    , health = 100
    , color = "rgba(66, 244, 223, "
    }


model : Model
model =
    { width = 1000
    , height = 1000
    , paddle =
        { x = 425
        , y = 940
        , vx = 0
        , vy = 0
        , width = 200
        , height = 25
        , hitbox =
            { xs = ( 400, 600 )
            , ys = ( 940, 965 )
            }
        , objectType = Paddle
        , health = 100
        , color = "gainsboro"
        }
    , ball =
        { x = 500
        , y = 600
        , vx = 0
        , vy = 7
        , width = 10
        , height = 10
        , hitbox =
            { xs = ( 500, 510 )
            , ys = ( 600, 610 )
            }
        , objectType = Ball
        , health = 100
        , color = "white"
        }
    , bricks = bricks
    , remainingLives = 3
    , wonGame = False
    }
