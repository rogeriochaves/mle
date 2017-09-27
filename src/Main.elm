module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Math.Matrix as Matrix exposing (..)
import Mle
import Mle.LinearRegression as LinearRegression
import Mle.Preprocessing exposing (..)
import Plot.Extra exposing (..)
import Random
import SampleData


main : Program Never (Maybe Int) (Maybe Int)
main =
    Mle.program run


data : Matrix Float
data =
    List.take 400 SampleData.data


run : Random.Seed -> Html msg
run seed =
    let
        scaledXs =
            unsafeGetColumns [ 1 ] data
                |> scaleMatrix

        ys =
            unsafeGetColumn 2 data

        ( trainXs, trainYs, testXs, testYs, _ ) =
            trainTestSplit scaledXs ys seed

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
