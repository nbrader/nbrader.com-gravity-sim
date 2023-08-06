module Helpers.ListHelpers exposing (toPairs)

toPairs : List a -> List (a, a)
toPairs lst =
    case lst of
        [] ->
            []

        [x] ->
            []

        x :: y :: rest ->
            (x, y) :: toPairs (y :: rest)