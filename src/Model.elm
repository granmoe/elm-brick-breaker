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
        vy : Float
    }

-- TODO
bricks : List GameObject
bricks = [
        { x = 0, y = 0, vx = 0, vy = 0 }
    ]

model : Model
model =
    { paddle = {
            x = 425,
            y = 940,
            vx = 0,
            vy = 0
        }
    , ball = {
            x = 500,
            y = 600,
            vx = 0,
            vy = 5
        }
    , bricks = bricks
    , lostGame = False
    , wonGame = False
    }
