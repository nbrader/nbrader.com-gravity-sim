module Linear.Matrix exposing (Matrix, mapFromAffineCoords, multiply, add, scale, subtract, transpose)

import List
import List.Extra
import Maybe

import Linear.Vectors.VecND exposing (Vector)

type alias Matrix = List Vector

multiply : Matrix -> Matrix -> Matrix
multiply matrixA matrixB =
    List.map 
        (\rowA -> 
            List.map 
                (\columnB -> List.sum (List.map2 (*) rowA columnB)) 
                (transpose matrixB)
        ) 
        matrixA

add : Matrix -> Matrix -> Matrix
add matrixA matrixB =
    List.map2 (List.map2 (+)) matrixA matrixB

scale : Float -> Matrix -> Matrix
scale scalar matrix =
    List.map (List.map ((*) scalar)) matrix

subtract : Matrix -> Matrix -> Matrix
subtract matrixA matrixB =
    List.map2 (List.map2 (-)) matrixA matrixB

transpose : Matrix -> Matrix
transpose matrix =
    case matrix of
        [] ->
            []
        
        ([] :: remainingRows) ->
            transpose remainingRows
            
        ((x :: xs) :: remainingRows) ->
            let
                (heads, tails) = List.unzip <| List.filterMap uncons remainingRows
            in
                (x :: heads) :: transpose (xs :: tails)

mapFromAffineCoords : Vector -> List Vector -> Vector -> Vector
mapFromAffineCoords affineCoordinates affineBasisVectors originVector =
    let
        originCoeff = 1 - List.sum affineCoordinates
        originTerm = List.map ((*) originCoeff) originVector
        basisTerms = List.map2 (\coord vector -> List.map ((*) coord) vector) affineCoordinates affineBasisVectors
    in
        List.map List.sum (transpose (originTerm :: basisTerms))

-- Helper function to calculate the sum of a list
uncons : List a -> Maybe (a, List a)
uncons lst =
    case lst of
        [] ->
            Nothing

        x :: xs ->
            Just (x, xs)
