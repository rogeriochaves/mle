module Mle.Preprocessing exposing (..)

import Helpers exposing (unwrap)
import Math.Matrix as Matrix exposing (..)
import NumElm
import Random
import Random.List


scaleMatrix : Matrix -> Matrix
scaleMatrix xs =
    let
        avg =
            NumElm.sum xs / toFloat (NumElm.length xs)

        standardDeviation =
            NumElm.max xs - NumElm.min xs

        standardDeviation_ =
            if standardDeviation == 0 then
                1
            else
                standardDeviation
    in
    NumElm.map (\x _ _ -> (x - avg) / standardDeviation_) xs


trainTestSplit : Matrix -> Vector -> Random.Generator ( Matrix, Vector, Matrix, Vector )
trainTestSplit xss ys =
    let
        xss_ =
            Matrix.toList xss |> unwrap

        ys_ =
            Matrix.toList ys |> unwrap

        eightyPercent =
            round (toFloat (List.length ys_) * 0.8)
    in
    List.map2 (\xs y -> ( xs, y )) xss_ ys_
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
        |> Random.map
            (\( trainXs, trainYs, testXs, testYs ) ->
                ( mat trainXs, mat trainYs, mat testXs, mat testYs )
            )
