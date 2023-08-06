module Curves.Ellipse exposing (Ellipse)

import Linear.Vectors.Vec2D exposing (Vec2D)

type alias Ellipse =
    { centre : Vec2D
    , a : Float
    , b : Float
    , rotation : Float
    }