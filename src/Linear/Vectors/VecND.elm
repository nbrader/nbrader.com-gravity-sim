module Linear.Vectors.VecND exposing (..)

import List

type alias Vector = List Float

add : Vector -> Vector -> Vector
add vecA vecB = List.map2 (+) vecA vecB

subtract : Vector -> Vector -> Vector
subtract vecA vecB = List.map2 (-) vecA vecB

scale : Float -> Vector -> Vector
scale scalar vec =
    List.map ((*) scalar) vec

div : Float -> Vector -> Vector
div divisor vec =
    List.map ((/) divisor) vec

floordiv : Float -> Vector -> Vector
floordiv divisor vec =
    List.map (\x -> toFloat (floor (x / divisor))) vec

neg : Vector -> Vector
neg vec =
    List.map negate vec

round : Vector -> Vector
round vec =
    List.map (Basics.round >> toFloat) vec

abs : Vector -> Vector
abs vec =
    List.map Basics.abs vec

zero : Int -> Vector
zero n = List.repeat n 0
