module Mle.Preprocessing exposing (..)

import Math.Matrix as Matrix exposing (..)
import Random
import Random.List


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


trainTestSplit : Matrix a -> Vector b -> Random.Generator ( Matrix a, Vector b, Matrix a, Vector b )
trainTestSplit xss ys =
    let
        eightyPercent =
            round (toFloat (List.length ys) * 0.8)
    in
    List.map2 (\xs y -> ( xs, y )) xss ys
        |> Random.List.shuffle
        |> Random.map
            (\xsYs ->
                let
                    xs_ =
                        List.map Tuple.first xsYs

                    ys_ =
                        List.map Tuple.second xsYs
                in
                ( List.take eightyPercent xs_, List.take eightyPercent ys_, List.drop eightyPercent xs_, List.drop eightyPercent ys_ )
            )
