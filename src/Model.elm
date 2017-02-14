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
    }


bricks : List GameObject
bricks =
    [ { x = 0
      , y = 0
      , vx = 0
      , vy = 0
      , width = 10
      , height = 10
      , hitbox =
            { xs = ( 0, 0 )
            , ys = ( 0, 0 )
            }
      , objectType = Brick
      }
    ]


model : Model
model =
    { width = 1000
    , height = 1000
    , paddle =
        { x = 425
        , y = 940
        , vx = 0
        , vy = 0
        , width = 150
        , height = 25
        , hitbox =
            { xs = ( 425, 575 )
            , ys = ( 940, 965 )
            }
        , objectType = Paddle
        }
    , ball =
        { x = 500
        , y = 600
        , vx = 4
        , vy = 5
        , width = 10
        , height = 10
        , hitbox =
            { xs = ( 500, 510 )
            , ys = ( 600, 610 )
            }
        , objectType = Ball
        }
    , bricks = bricks
    , remainingLives = 3
    , wonGame = False
    }
