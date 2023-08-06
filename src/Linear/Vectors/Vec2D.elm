module Linear.Vectors.Vec2D exposing (Vec2D, add, scale, norm, subtract, normalize, distance, dot, decomposeVec2D, zero)

import Basics exposing (sqrt)

type alias Vec2D =
    { x : Float
    , y : Float
    }

add : Vec2D -> Vec2D -> Vec2D
add v1 v2 =
    { x = v1.x + v2.x, y = v1.y + v2.y }

scale : Float -> Vec2D -> Vec2D
scale scalar v =
    { x = scalar * v.x, y = scalar * v.y }

norm : Vec2D -> Float
norm v =
    sqrt (v.x * v.x + v.y * v.y)

subtract : Vec2D -> Vec2D -> Vec2D
subtract v1 v2 =
    { x = v1.x - v2.x, y = v1.y - v2.y }

distance : Vec2D -> Vec2D -> Float
distance pos1 pos2 =
    let
        diff = subtract pos1 pos2
    in
    norm diff

normalize : Vec2D -> Vec2D
normalize v =
    let
        length = norm v
    in
    if length == 0 then
        { x = 0, y = 0 }
    else
        scale (1 / length) v

dot : Vec2D -> Vec2D -> Float
dot v1 v2 =
    v1.x * v2.x + v1.y * v2.y

decomposeVec2D : Vec2D -> Vec2D -> (Vec2D, Vec2D)
decomposeVec2D vector dir =
    let
        alignedComponent = scale (dot vector dir) dir
        orthogonalComponent = subtract vector alignedComponent
    in
    (alignedComponent, orthogonalComponent)

zero = {x=0, y=0}