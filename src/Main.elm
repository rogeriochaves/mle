module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Math.Matrix as Matrix exposing (..)
import Mle.LinearRegression as LinearRegression exposing (defaultSettings)
import Mle.Preprocessing exposing (..)
import Platform exposing (Task)
import Plot.Extra exposing (..)
import Random.Extra.Task
import SampleData
import Task
import Task.Extra


main : Program Never (Maybe (Html Task.Extra.Msg)) Task.Extra.Msg
main =
    Task.Extra.taskProgram run


data : Matrix Float
data =
    List.take 400 SampleData.data


run : Task Never (Html Task.Extra.Msg)
run =
    let
        scaledXs =
            unsafeGetColumns [ 1 ] data
                |> scaleMatrix

        ys =
            unsafeGetColumn 2 data
    in
    trainTestSplit scaledXs ys
        |> Random.Extra.Task.toTask
        |> Task.map
            (\( trainXs, trainYs, testXs, testYs ) ->
                let
                    predictions =
                        LinearRegression.init { defaultSettings | learningRate = 1 }
                            |> LinearRegression.train trainXs trainYs
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
            )
