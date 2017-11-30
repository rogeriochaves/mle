module Examples.LinearRegression exposing (..)

import Csv
import Csv.Decode
import Helpers exposing (unwrap)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Math.Matrix as Matrix exposing (..)
import Mle.LinearRegression as LinearRegression exposing (defaultSettings)
import Mle.Preprocessing exposing (..)
import Platform exposing (Task)
import Plot.Extra exposing (..)
import TaskIO exposing (..)


main : Program Never (Maybe (Html TaskIO.Msg)) TaskIO.Msg
main =
    TaskIO.program run


run : Task String (Html TaskIO.Msg)
run =
    returnHttp (Http.getString "http://localhost:8000/examples/linearSample.txt")
        |> andThen parseCsv
        |> andThen splitData
        |> andThen trainAndPredict
        |> andThen plotResults


parseCsv : String -> TaskIO Matrix
parseCsv csv =
    let
        descodeSampleData =
            Csv.Decode.map (\index x y -> [ index, x, y ])
                (Csv.Decode.next String.toFloat
                    |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
                    |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
                )
    in
    Csv.parse csv
        |> Csv.Decode.decodeCsv descodeSampleData
        |> Result.map (List.take 400 >> Matrix.mat)
        |> returnResult


splitData : Matrix -> TaskIO ( Matrix, Vector, Matrix, Vector )
splitData data =
    let
        scaledXs =
            unsafeGetColumns [ 1 ] data
                |> scaleMatrix

        ys =
            unsafeGetColumn 2 data
    in
    returnRandom (trainTestSplit scaledXs ys)


trainAndPredict : ( Matrix, Vector, Matrix, a ) -> TaskIO ( Matrix, Vector, Matrix, Vector )
trainAndPredict ( trainXs, trainYs, testXs, testYs ) =
    LinearRegression.init { defaultSettings | learningRate = 1 }
        |> LinearRegression.train trainXs trainYs
        |> LinearRegression.predict testXs
        |> Result.map (\predictions -> ( trainXs, trainYs, testXs, predictions ))
        |> returnResult


plotResults : ( Matrix, Vector, Matrix, Vector ) -> TaskIO (Html msg)
plotResults ( trainXs, trainYs, testXs, predictions ) =
    return <|
        div [ style [ ( "width", "800px" ), ( "height", "800px" ) ] ]
            [ plotSeries
                [ scatter (trainXs |> unsafeGetColumn 0 |> Matrix.vectorToList |> unwrap) (Matrix.vectorToList trainYs |> unwrap)
                , plot (testXs |> unsafeGetColumn 0 |> Matrix.vectorToList |> unwrap) (Matrix.vectorToList predictions |> unwrap)
                ]
            ]
