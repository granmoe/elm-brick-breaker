module RectangleIntersection exposing (rectangleIntersection)


type alias Rectangle =
    { xs : ( Float, Float )
    , ys : ( Float, Float )
    }


lineIntersection : ( Float, Float ) -> ( Float, Float ) -> Float
lineIntersection ( lineOneStart, lineOneEnd ) ( lineTwoStart, lineTwoEnd ) =
    {--in our case, we can assume that both tuples will always be sorted from low to high --}
    min lineOneEnd lineTwoEnd - max lineOneStart lineTwoStart



-- can give each brick a UID
-- remove paddle/brick/ball types and instead just give each gameobject a type field (they're basically the same anyway)


rectangleIntersection : Rectangle -> Rectangle -> ( Float, Float )
rectangleIntersection rectOne rectTwo =
    ( lineIntersection rectOne.xs rectTwo.xs, lineIntersection rectOne.ys rectTwo.ys )
