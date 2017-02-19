module View exposing (view)

import Html exposing (Html, div, Attribute)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Model exposing (model, Model, GameObject)


wrapperStyle : Html.Attribute msg
wrapperStyle =
    Html.Attributes.style
        [ ( "backgroundColor", "papayawhip" )
        , ( "position", "fixed" )
        , ( "top", "0" )
        , ( "right", "0" )
        , ( "bottom", "0" )
        , ( "left", "0" )
        , ( "padding", "10%" )
        ]


view : Model -> Html msg
view model =
    let
        gameWidth =
            toString model.width

        gameHeight =
            toString model.height

        gameScreen =
            rect
                [ fill "black", x "0", y "0", width gameWidth, height gameHeight ]
                []
    in
        div [ wrapperStyle ]
            [ svg
                [ width "100%", height "100%", viewBox <| "0 0 " ++ gameWidth ++ " " ++ gameHeight ]
                (List.append [ gameScreen ] <| renderChildren model)
            ]


renderChildren : Model -> List (Html msg)
renderChildren model =
    List.append [ renderPaddle model.paddle, renderBall model.ball ] <| List.map renderBrick model.bricks


renderPaddle : GameObject -> Html msg
renderPaddle paddle =
    rect
        [ fill paddle.color, x <| toString paddle.x, y <| toString paddle.y, width <| toString paddle.width, height <| toString paddle.height, rx "5", ry "5" ]
        []


renderBall : GameObject -> Html msg
renderBall ball =
    circle
        [ fill ball.color, cx <| toString ball.x, cy <| toString ball.y, r <| toString ball.width ]
        []


renderBrick : GameObject -> Html msg
renderBrick brick =
    rect
        [ fill <| brick.color, fillOpacity <| getBrickOpacity brick, x <| toString brick.x, y <| toString brick.y, width <| toString brick.width, height <| toString brick.height ]
        []


getBrickOpacity : GameObject -> String
getBrickOpacity brick =
    brick.health / 100 |> toString
