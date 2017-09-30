module Examples.LogisticRegression exposing (..)

import Csv
import Csv.Decode
import Helpers
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import List.Extra
import Math.Matrix as Matrix exposing (..)
import Math.Vector as Vector exposing (..)
import Mle.LogisticRegression as LogisticRegression exposing (defaultSettings)
import Mle.Preprocessing exposing (..)
import Platform exposing (Task)
import Plot
import Plot.Extra exposing (..)
import TaskIO exposing (..)


main : Program Never (Maybe (Html TaskIO.Msg)) TaskIO.Msg
main =
    TaskIO.program run


run : Task String (Html TaskIO.Msg)
run =
    returnHttp (Http.getString "http://localhost:8000/examples/binarySample.txt")
        |> andThen parseCsv
        |> andThen splitData
        |> andThen trainAndPredict
        |> andThen plotResults


parseCsv : String -> TaskIO (Matrix Float)
parseCsv csv =
    let
        descodeSampleData =
            Csv.Decode.map (\tumor_size growth is_cancer -> [ tumor_size, growth, is_cancer ])
                (Csv.Decode.next String.toFloat
                    |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
                    |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
                )
    in
    Csv.parse csv
        |> Csv.Decode.decodeCsv descodeSampleData
        |> returnResult


splitData : Matrix Float -> TaskIO ( Matrix Float, Vector Float, Matrix Float, Vector Float )
splitData data =
    let
        scaledXs =
            unsafeGetColumns [ 0, 1 ] data
                |> scaleMatrix

        ys =
            unsafeGetColumn 2 data
    in
    returnRandom (trainTestSplit scaledXs ys)


trainAndPredict : ( Matrix Float, Vector Float, Matrix Float, a ) -> TaskIO ( Matrix Float, Vector Float, Matrix Float, Vector Float )
trainAndPredict ( trainXs, trainYs, testXs, testYs ) =
    LogisticRegression.init defaultSettings
        |> LogisticRegression.train trainXs trainYs
        |> LogisticRegression.predict testXs
        |> Result.map (\predictions -> ( trainXs, trainYs, testXs, predictions ))
        |> returnResult


plotResults : ( Matrix Float, Vector Float, Matrix Float, Vector Float ) -> TaskIO (Html msg)
plotResults ( trainXs, trainYs, testXs, predictions ) =
    let
        xs =
            trainXs ++ testXs

        ys =
            trainYs ++ predictions
    in
    return <|
        div [ style [ ( "width", "800px" ), ( "height", "800px" ) ] ]
            [ plotSeries
                [ Plot.dots
                    (\_ ->
                        List.map2
                            (\xs y ->
                                let
                                    color =
                                        if round y == 1 then
                                            "red"
                                        else
                                            "green"

                                    d1 =
                                        List.Extra.getAt 0 xs |> Helpers.unwrapMaybe "could not get first x"

                                    d2 =
                                        List.Extra.getAt 1 xs |> Helpers.unwrapMaybe "could not get second x"
                                in
                                Plot.dot (Plot.viewCircle 5 color) (d1 + 1) (d2 + 1)
                            )
                            xs
                            ys
                    )
                ]
            ]
