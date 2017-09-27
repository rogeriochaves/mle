module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Math.Matrix as Matrix exposing (..)
import Mle.LinearRegression as LinearRegression
import Mle.Preprocessing exposing (..)
import Plot.Extra exposing (..)
import Random
import SampleData


data : Matrix Float
data =
    List.take 400 SampleData.data


main : Html msg
main =
    let
        scaledXs =
            unsafeGetColumns [ 1 ] data
                |> transpose
                |> scaleMatrix

        ys =
            unsafeGetColumn 2 data

        randomSeed =
            Random.initialSeed 1

        ( trainXs, trainYs, testXs, testYs, _ ) =
            trainTestSplit scaledXs ys randomSeed

        predictions =
            LinearRegression.train trainXs trainYs
                |> LinearRegression.predict testXs
    in
    case predictions of
        Ok predictions ->
            div [ style [ ( "width", "800px" ), ( "height", "800px" ) ] ]
                [ plotSeries
                    [ scatter (trainXs |> unsafeGetColumn 0) trainYs
                    , plot (testXs |> unsafeGetColumn 0) predictions
                    ]
                ]

        Err err ->
            text err
