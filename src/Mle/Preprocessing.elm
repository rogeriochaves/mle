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


trainTestSplit : Matrix a -> Vector b -> Random.Seed -> ( Matrix a, Vector b, Matrix a, Vector b, Random.Seed )
trainTestSplit xs ys seed =
    let
        eightyPercent =
            round (toFloat (List.length xs) * 0.8)

        ( xs_, _ ) =
            xs
                |> Random.List.shuffle
                |> flip Random.step seed

        ( ys_, seed_ ) =
            ys
                |> Random.List.shuffle
                |> flip Random.step seed
    in
    ( List.take eightyPercent xs_, List.take eightyPercent ys_, List.drop eightyPercent xs_, List.drop eightyPercent ys_, seed_ )
