module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import LinearRegression
import Math.Matrix as Matrix exposing (..)
import Plot exposing (..)
import Preprocessing exposing (scaleMatrix)
import Random
import Random.List
import SampleData
import Unwrap exposing (..)


type Msg
    = RandomData (Matrix Float)


type alias Model =
    { data : Matrix Float, predictions : Result String (Matrix Float) }


model : Model
model =
    { data = List.take 400 SampleData.data, predictions = Err "No model yet" }


initialCmd : Cmd Msg
initialCmd =
    model.data
        |> Random.List.shuffle
        |> Random.generate RandomData


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        RandomData data ->
            let
                scaledXs =
                    getColumns [ 1 ] data
                        |> unwrap "could not get xs"
                        |> transpose
                        |> scaleMatrix

                eightyPercent =
                    round (toFloat (List.length scaledXs) * 0.8)

                trainXs =
                    List.take eightyPercent scaledXs

                trainYs =
                    getColumn 2 data
                        |> unwrap "could not get ys"
                        |> List.take eightyPercent

                testXs =
                    List.drop eightyPercent scaledXs

                unscaledTestXs =
                    getColumns [ 1 ] data
                        |> unwrap "could not get xs"
                        |> transpose
                        |> List.drop eightyPercent

                predictions =
                    LinearRegression.train trainXs trainYs
                        |> LinearRegression.predict testXs
                        |> Result.map (List.map2 (\testX prediction -> testX ++ [ prediction ]) unscaledTestXs)
            in
            ( { model | data = data, predictions = predictions }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, initialCmd )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


view : Model -> Html msg
view model =
    case model.predictions of
        Err err ->
            text err

        Ok predictions ->
            let
                plotData =
                    ( Matrix.getColumn 1 model.data
                    , Matrix.getColumn 2 model.data
                    , Matrix.getColumn 0 predictions
                    , Matrix.getColumn 1 predictions
                    )
            in
            case plotData of
                ( Just xsData, Just ysData, Just xsPred, Just ysPred ) ->
                    div [ style [ ( "width", "50%" ), ( "height", "50%" ) ] ]
                        [ viewSeries
                            [ dots (\( xs, ys, _, _ ) -> List.map2 circle xs ys)
                            , line (\( _, _, xs, ys ) -> List.map2 Plot.clear xs ys)
                            ]
                            ( xsData, ysData, xsPred, ysPred )
                        ]

                _ ->
                    text "bar"
