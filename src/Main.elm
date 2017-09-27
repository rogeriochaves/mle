module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import LinearRegression
import Math.Matrix as Matrix exposing (..)
import Plot exposing (..)
import Preprocessing exposing (scaleMatrix)
import Random
import SampleData
import Unwrap exposing (..)


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
            Preprocessing.trainTestSplit scaledXs ys randomSeed

        predictions =
            LinearRegression.train trainXs trainYs
                |> LinearRegression.predict testXs
                |> Result.toMaybe
                |> unwrap "failed linear regression"
    in
    div [ style [ ( "width", "800px" ), ( "height", "800px" ) ] ]
        [ viewSeries
            [ scatter (trainXs |> unsafeGetColumn 0) trainYs
            , plot (testXs |> unsafeGetColumn 0) predictions
            ]
            []
        ]


scatter : List Float -> List Float -> Series b msg
scatter xs ys =
    dots (\_ -> List.map2 circle xs ys)


plot : List Float -> List Float -> Series b msg
plot xs ys =
    line (\_ -> List.map2 clear xs ys)
