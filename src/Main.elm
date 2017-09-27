module Main exposing (..)

import Csv
import Csv.Decode
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Math.Matrix as Matrix exposing (..)
import Mle.LinearRegression as LinearRegression exposing (defaultSettings)
import Mle.Preprocessing exposing (..)
import Platform exposing (Task)
import Plot.Extra exposing (..)
import Random.Extra.Task
import Task
import Task.Extra


main : Program Never (Maybe (Html Task.Extra.Msg)) Task.Extra.Msg
main =
    Task.Extra.taskProgram run


descodeSampleData : Csv.Decode.Decoder (List Float -> a) a
descodeSampleData =
    Csv.Decode.map (\index x y -> [ index, x, y ])
        (Csv.Decode.next String.toFloat
            |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
            |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
        )


run : Task String (Html Task.Extra.Msg)
run =
    Http.getString "http://localhost:8000/sampleData.txt"
        |> Http.toTask
        |> Task.mapError toString
        |> Task.andThen
            (\csv ->
                Csv.parse csv
                    |> Csv.Decode.decodeCsv descodeSampleData
                    |> Result.map (List.take 400)
                    |> Result.mapError toString
                    |> Helpers.resultToTask
            )
        |> Task.andThen
            (\data ->
                let
                    scaledXs =
                        unsafeGetColumns [ 1 ] data
                            |> scaleMatrix

                    ys =
                        unsafeGetColumn 2 data
                in
                trainTestSplit scaledXs ys
                    |> Random.Extra.Task.toTask
                    |> Task.mapError toString
            )
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
