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
    returnHttp (Http.getString "http://localhost:8000/examples/irisFlowerDataset.txt")
        |> andThen parseCsv
        |> andThen splitData
        |> andThen trainAndPredict
        |> andThen plotResults


parseCsv : String -> TaskIO (Matrix Float)
parseCsv csv =
    let
        petalIndex name =
            List.Extra.elemIndex name [ "I. setosa", "I. versicolor", "I. virginica" ]
                |> Maybe.map toFloat
                |> Result.fromMaybe "Unknown species"

        descodeSampleData =
            Csv.Decode.map
                (\sepal_length sepal_width petal_length petal_width species ->
                    [ sepal_length, sepal_width, petal_length, petal_width, species ]
                )
                (Csv.Decode.next String.toFloat
                    |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
                    |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
                    |> Csv.Decode.andMap (Csv.Decode.next String.toFloat)
                    |> Csv.Decode.andMap (Csv.Decode.next petalIndex)
                )
    in
    Csv.parse csv
        |> Csv.Decode.decodeCsv descodeSampleData
        |> returnResult


splitData : Matrix Float -> TaskIO ( Matrix Float, Vector Float, Matrix Float, Vector Float )
splitData data =
    let
        scaledXs =
            unsafeGetColumns [ 0, 1, 2, 3 ] data
                |> scaleMatrix

        ys =
            unsafeGetColumn 4 data
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

        colors =
            [ "red", "green", "blue" ]

        dotAt xs y =
            let
                color =
                    List.Extra.getAt (round y) colors |> Helpers.unwrapMaybe "no color found"

                sepal_length =
                    List.Extra.getAt 0 xs |> Helpers.unwrapMaybe "could not get first x"

                sepal_width =
                    List.Extra.getAt 1 xs |> Helpers.unwrapMaybe "could not get second x"
            in
            Plot.dot (Plot.viewCircle 5 color) (sepal_length + 1) (sepal_width + 1)
    in
    return <|
        div [ style [ ( "width", "800px" ), ( "height", "800px" ) ] ]
            [ plotSeries
                [ Plot.dots (\_ -> List.map2 dotAt xs ys) ]
            ]
