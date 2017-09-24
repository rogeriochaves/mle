module Preprocessing exposing (..)

import Math.Matrix as Matrix exposing (..)


scaleMatrix : Matrix Float -> Matrix Float
scaleMatrix xs =
    Matrix.transpose xs
        |> List.map scaleVector
        |> Matrix.transpose


scaleVector : Vector Float -> Vector Float
scaleVector xs =
    let
        avg =
            List.sum xs / toFloat (List.length xs)

        standardDeviation =
            Maybe.withDefault 0 (List.maximum xs)
                - Maybe.withDefault 0 (List.minimum xs)

        standardDeviation_ =
            if standardDeviation == 0 then
                1
            else
                standardDeviation
    in
    List.map (\x -> (x - avg) / standardDeviation_) xs
