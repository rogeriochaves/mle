module Main exposing (..)

import Html exposing (..)
import LinearRegression exposing (..)
import Math.Matrix exposing (..)
import Preprocessing exposing (scaleMatrix)
import Unwrap exposing (..)


data : Matrix Float
data =
    [ [ 1.0, 2.5 ]
    , [ 2.0, 4.5 ]
    , [ 3.0, 6.5 ]
    , [ 4.0, 8.5 ]
    , [ 5.0, 10.5 ]
    ]


main : Html msg
main =
    let
        scaledXs =
            getColumns [ 0 ] data
                |> unwrap "could not get xs"
                |> transpose
                |> scaleMatrix

        trainXs =
            List.take 3 scaledXs

        trainYs =
            getColumn 1 data
                |> unwrap "could not get ys"
                |> List.take 3

        testXs =
            List.drop 3 scaledXs
    in
    linearRegression trainXs trainYs
        |> (\model -> model.train ())
        |> Result.andThen (\model -> model.predict testXs)
        |> toString
        |> text
