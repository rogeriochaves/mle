module Regression exposing (..)

import DualNumber exposing (..)


hypotesis : DualNumber -> DualNumber -> DualNumber
hypotesis parameter1 x =
    parameter1 * x


squaredError : List ( Float, Float ) -> DualNumber -> DualNumber
squaredError data parameter1 =
    let
        dataSize =
            toDual (toFloat (List.length data))

        sumHypotesis =
            data
                |> List.map (\( x, y ) -> ( toDual x, toDual y ))
                |> List.map (\( x, y ) -> (hypotesis parameter1 x - y) ^ 2)
                |> List.foldl sum (toDual 0)
    in
    (one / (two * dataSize)) * sumHypotesis



--


(+) : DualNumber -> DualNumber -> DualNumber
(+) =
    DualNumber.sum


(-) : DualNumber -> DualNumber -> DualNumber
(-) =
    subtract


(^) : DualNumber -> Float -> DualNumber
(^) =
    pow


(*) : DualNumber -> DualNumber -> DualNumber
(*) =
    multiply


(/) : DualNumber -> DualNumber -> DualNumber
(/) =
    divide
