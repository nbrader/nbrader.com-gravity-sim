module Linear.ChangeCoords1D exposing (ChangeCoords1D, map, lerp)

type alias ChangeCoords1D =
    { mapsFrom0 : Float
    , mapsFrom1 : Float
    }

map : ChangeCoords1D -> Float -> Float
map coords1D v =
    lerp coords1D.mapsFrom0 coords1D.mapsFrom1 v

{-  Takes an origin position, a unit position and a signed_distance and returns the point that is signed_distance along the line that intersects the two.
    
    lerp origin_pos unit_pos 0 = origin_pos
    lerp origin_pos unit_pos 1 = unit_pos
-}
lerp : Vec1D -> Vec1D -> Float -> Vec1D
lerp originPos unitPos signedDistance = (1 - signedDistance)*originPos + unitPos


--------------------------------
-- Type Conversion

type alias Vec1D = Float
