module Rectangle exposing (rectangleIntersection, Rectangle, lineIntersection)


type alias Rectangle =
    { xs : ( Float, Float )
    , ys : ( Float, Float )
    }


lineIntersection : ( Float, Float ) -> ( Float, Float ) -> Float
lineIntersection ( lineOneStart, lineOneEnd ) ( lineTwoStart, lineTwoEnd ) =
    {--in our case, we can assume that both tuples will always be sorted from low to high --}
    let
        offset =
            min lineOneEnd lineTwoEnd - max lineOneStart lineTwoStart
    in
        if (offset < 0) then
            0
        else
            offset


rectangleIntersection : Rectangle -> Rectangle -> { x : Float, y : Float, intersects : Bool }
rectangleIntersection rectOne rectTwo =
    let
        x =
            lineIntersection rectOne.xs rectTwo.xs

        y =
            lineIntersection rectOne.ys rectTwo.ys
    in
        if (x > 0 && y > 0) then
            { x = x
            , y = y
            , intersects = True
            }
        else
            { x = 0
            , y = 0
            , intersects = False
            }
