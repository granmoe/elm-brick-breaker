module Config exposing (config, GameProgress(..))


type GameProgress
    = Playing
    | Won
    | Lost


type alias Config =
    { brickWidth : Float
    , brickHeight : Float
    , brickHealth : Float
    , defaultBrickColor : String
    , paddleWidth : Float
    , paddleHeight : Float
    , paddleInitY : Float
    , paddleHealth : Float
    , paddleColor : String
    , ballDiameter : Float
    , ballInitY : Float
    , ballHealth : Float
    , ballColor : String
    , ballInitVY : Float
    , gameWidth : Float
    , gameHeight : Float
    , brickCount : Int
    , remainingLives : Int
    , gameProgress : GameProgress
    }


config : Config
config =
    { brickWidth = 200
    , brickHeight = 50
    , brickHealth = 100
    , defaultBrickColor = "#fff"
    , paddleWidth = 200
    , paddleHeight = 35
    , paddleInitY = 930
    , paddleHealth = 100
    , paddleColor = "gainsboro"
    , ballDiameter = 15
    , ballInitY = 600
    , ballHealth = 100
    , ballColor = "white"
    , ballInitVY = 7
    , gameWidth = 1000
    , gameHeight = 1000
    , brickCount = 20
    , remainingLives = 3
    , gameProgress = Playing
    }



-- TODO: Try using a smaller ball and see if weird collisions happen less frequently
