module Model exposing (Model, model, GameObject)

-- MODEL
type alias Model =
    { paddle : GameObject
    , ball : GameObject
    , bricks : List GameObject
    , lostGame : Bool
    , wonGame : Bool
    }

type alias GameObject = 
    {
        x : Float,
        y : Float,
        vx : Float, 
        vy : Float,
        width: Float,
        height: Float
    }

-- TODO
bricks : List GameObject
bricks = [
        { x = 0, y = 0, vx = 0, vy = 0, width = 10, height = 10 }
    ]

model : Model
model =
    { paddle = {
            x = 425,
            y = 940,
            vx = 0,
            vy = 0,
            width = 150,
            height = 25 
        }
    , ball = {
            x = 500,
            y = 600,
            vx = 0,
            vy = 5,
            width = 10,
            height = 10
        }
    , bricks = bricks
    , lostGame = False
    , wonGame = False
    }
