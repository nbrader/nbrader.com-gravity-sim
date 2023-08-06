module Bezier exposing (getBezierCurvePoint)

import Linear.Vectors.Vec2D exposing (Vec2D)
import Linear.ChangeCoords2D exposing (lerp)
import Helpers.ListHelpers exposing (toPairs)

getBezierCurvePoint : List Vec2D -> Float -> Vec2D
getBezierCurvePoint controlPoints parameter =
    case controlPoints of
        [] ->
            {x = 0, y = 0}

        [ singlePoint ] ->
            singlePoint

        _ ->
            let
                interpolatedPoints =
                    controlPoints
                        |> toPairs
                        |> List.map (\( initialPoint, finalPoint ) -> lerp initialPoint finalPoint parameter)
            in
            getBezierCurvePoint interpolatedPoints parameter