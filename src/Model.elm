module Model exposing (Model, model, generateBricks, GameObject, GameObjectName(Brick, Paddle, Ball), CollisionData, collisionData)

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
    , brickCount : Int
    , remainingLives : Int
    , wonGame : Bool
    , lostGame : Bool
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
            if (x == 800) then
                0
            else
                x + 200
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
                    (if (x == 800) then
                        y + 50
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
                    "#fff"
    in
        { brickTemplate
            | color = color_
            , x = x
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
    , color = ""
    }


model : Model
model =
    { width = 1000
    , height = 1000
    , paddle =
        { x = 400
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
    , bricks = []
    , remainingLives = 3
    , wonGame = False
    , lostGame = False
    , brickCount = 20
    }
