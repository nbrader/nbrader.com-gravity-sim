module Linear.ChangeCoords2D exposing (ChangeCoords2D, map, planarLerp, lerp, invLerp, vec2DToVector, vectorToVec2D)

import Linear.Vectors.Vec2D exposing (Vec2D)
import Linear.Vectors.VecND exposing (Vector)
import Linear.Matrix exposing (mapFromAffineCoords)

type alias ChangeCoords2D =
    { mapsFrom00 : Vec2D
    , mapsFrom10 : Vec2D
    , mapsFrom01 : Vec2D
    }

map : ChangeCoords2D -> Vec2D -> Vec2D
map coords2D v =
    planarLerp coords2D.mapsFrom00 coords2D.mapsFrom10 coords2D.mapsFrom01 v


{- Takes an origin position, x and y unit positions and a pair of x and y coordinates and returns the position that is at the x and y coordinates in terms of the coordinate system with the provided origin and basis vectors as difference vectors of the x and y unit positions from the origin position.
    
    f(x, y) := planarLerp {x=x, y=y}
    
    Boundary conditions:
        f(0, 0) == origin_pos
        f(1, 0) == x_unit_pos
        f(0, 1) == y_unit_pos
    
    Bilinearity:
        f(x+z, y) == f(x, y) + f(z, y)
        f(x, y+z) == f(x, y) + f(x, z)
        f(a*x, y) == f(x, a*y) == a*f(x, y)
-}
planarLerp : Vec2D -> Vec2D -> Vec2D -> Vec2D -> Vec2D
planarLerp originPos xUnitPos yUnitPos xyCoords =
    mapFromAffineCoords (vec2DToVector xyCoords) [ vec2DToVector xUnitPos, vec2DToVector yUnitPos ] (vec2DToVector originPos)
        |> vectorToVec2D


{-  Takes an origin position, u and v unit positions in natural coords and input natural coords and returns in affine coords the point corresponding to these natural coords using the provided origin and basis.
    
    invPlanarLerp origin_pos u_unit_pos v_unit_pos origin_pos = {x=0, y=0}
    invPlanarLerp origin_pos u_unit_pos v_unit_pos u_unit_pos = {x=1, y=0}
    invPlanarLerp origin_pos u_unit_pos v_unit_pos v_unit_pos = {x=0, y=1}
-}
invPlanarLerp : Vec2D -> Vec2D -> Vec2D -> Vec2D -> Maybe Vec2D
invPlanarLerp originPos uUnitPos vUnitPos naturalCoords =
    let
        det =
            (vUnitPos.y - originPos.y) * (uUnitPos.x - originPos.x) + (originPos.x - vUnitPos.x) * (uUnitPos.y - originPos.y)
    in
    if det == 0 then
        Nothing
    else
        let
            naturalCoordsX =
                ((vUnitPos.y - originPos.y) * (naturalCoords.x - originPos.x) + (originPos.x - vUnitPos.x) * (naturalCoords.y - originPos.y)) / det

            naturalCoordsY =
                ((originPos.y - uUnitPos.y) * (naturalCoords.x - originPos.x) + (uUnitPos.x - originPos.x) * (naturalCoords.y - originPos.y)) / det
        in
        Just { x = naturalCoordsX, y = naturalCoordsY }


{-  Takes an origin position, a unit position and a signed_distance and returns the point that is signed_distance along the line that intersects the two.
    
    lerp origin_pos unit_pos 0 = origin_pos
    lerp origin_pos unit_pos 1 = unit_pos
-}
lerp : Vec2D -> Vec2D -> Float -> Vec2D
lerp originPos unitPos signedDistance =
    mapFromAffineCoords [ signedDistance ] [ vec2DToVector unitPos ] (vec2DToVector originPos)
        |> vectorToVec2D

{-  Takes an origin position, a unit position and a colinear_vector and returns the signed distance from the origin position to the colinear_vector along the line that intersects the two.
    
    f(x) := lerp(origin_pos, unit_pos, x)
    f^-1(x) := invLerp(origin_pos, unit_pos, x)
-}
invLerp : Vec2D -> Vec2D -> Vec2D -> Float
invLerp originPos unitPos colinearVector =
    (colinearVector.x - originPos.x) / (unitPos.x - originPos.x)


--------------------------------
-- Type Conversion

vec2DToVector : Vec2D -> Vector
vec2DToVector v = [v.x, v.y]

vectorToVec2D : Vector -> Vec2D
vectorToVec2D vector =
    case vector of
        [x, y] -> {x = x, y = y}
        _      -> {x = 0, y = 0}  -- Or any other suitable default
