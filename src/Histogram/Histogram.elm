module Histogram.Histogram exposing (createBins)

import Maybe
import List.Extra

createBins : Float -> Float -> Int -> List Float -> List (Int, Int)
createBins minVal binWidth numBins values =
    let
        emptyBins = List.repeat numBins 0
    in
    List.foldl
        (\value bins ->
            let
                binIndex = min (numBins - 1) <| floor ((value - minVal) / binWidth)
                count = Maybe.withDefault 0 <| List.Extra.getAt binIndex bins
                updatedBins = List.Extra.setAt binIndex (count + 1) bins
            in
            updatedBins
        )
        emptyBins
        values
    |> List.indexedMap Tuple.pair
