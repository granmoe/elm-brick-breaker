module Model exposing (Model, model, GameObject, Edge)


type alias Model =
    { width : Float
    , height : Float
    , paddle : GameObject
    , ball : GameObject
    , bricks : List GameObject
    , lostGame : Bool
    , wonGame : Bool
    }


type alias Line =
    -- (x intersect, y start, y end) or
    -- (y intersect, x start, x end)
    ( Float, Float, Float )


type Edge
    = Top Line
    | Right Line
    | Bottom Line
    | Left Line


type alias Hitbox =
    -- [(x1, x2), (y1, y2)]
    List ( Float, Float )


type alias GameObject =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , width : Float
    , height : Float
    , hitbox : Hitbox
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
            [ ( 0, 0 )
            , ( 0, 0 )
            ]
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
            [ ( 425, 575 )
            , ( 940, 965 )
            ]
        }
    , ball =
        { x = 500
        , y = 600
        , vx = 0
        , vy = 5
        , width = 10
        , height = 10
        , hitbox =
            [ ( 500, 510 )
            , ( 600, 610 )
            ]
        }
    , bricks = bricks
    , lostGame = False
    , wonGame = False
    }
