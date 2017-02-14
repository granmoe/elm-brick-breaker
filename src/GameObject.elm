module Main exposing (..)


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
