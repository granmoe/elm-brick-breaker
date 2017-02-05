module View exposing (view)

import Html exposing (Html, div, Attribute )
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Model exposing (model, Model, GameObject)

wrapperStyle : Html.Attribute msg
wrapperStyle =
    Html.Attributes.style
    [ ("backgroundColor", "papayawhip")
    , ("position", "fixed")
    , ("top", "0")
    , ("right", "0")
    , ("bottom", "0")
    , ("left", "0")
    , ("padding", "10%")
    ]

view : Model -> Html msg
view model =
    let
        gameWidth = toString model.width
        gameHeight = toString model.height
    in
        div [ wrapperStyle ]
            [
                svg
                    [ width "100%", height "100%", viewBox <| "0 0 " ++ gameWidth ++ " " ++ gameHeight ]
                    [
                        rect
                        [ fill "black", x "0", y "0", width gameWidth, height gameHeight ]
                        []
                        , paddle model
                        , ball model
                    ]
            ]

paddle : Model -> Html msg
paddle model =
    rect
    [ fill "gainsboro", x <| toString model.paddle.x, y <| toString model.paddle.y, width <| toString model.paddle.width, height <| toString model.paddle.height, rx "5", ry "5" ]
    []

ball : Model -> Html msg
ball model =
    circle
    [ fill "white", cx <| toString model.ball.x, cy <| toString model.ball.y, r <| toString model.ball.width ]
    []

