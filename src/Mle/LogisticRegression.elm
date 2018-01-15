module Mle.LogisticRegression exposing (..)

import Helpers
import List.Extra
import Math.Constants
import Math.Matrix as Matrix exposing (..)
import Math.Vector as Vector exposing (..)
import Mle.Internal.Regression as Internal


type alias Model =
    Result String { settings : Settings, models : List ClassModel }


type alias ClassModel =
    { class : Float
    , params : Vector Float
    }


type alias Settings =
    Internal.Settings


defaultSettings : Settings
defaultSettings =
    Internal.defaultSettings


init : Settings -> Model
init settings =
    Ok { settings = settings, models = [] }


train : Matrix Float -> Vector Float -> Model -> Model
train xs ys model =
    model
        |> Result.andThen
            (\model ->
                oneVsAllLabels ys
                    |> List.map (trainOneClass model.settings xs)
                    |> Helpers.flatResultList
                    |> Result.map (\models -> { settings = model.settings, models = models })
            )


trainOneClass : Internal.Settings -> Matrix Float -> { class : Float, ys : Vector Float } -> Result String ClassModel
trainOneClass settings xs { class, ys } =
    Internal.train hypotesis xs ys (Internal.init settings)
        |> Result.map (\{ params } -> { class = class, params = params })


predict : Matrix Float -> Model -> Result String (Vector Float)
predict xs =
    predict_ xs >> Result.map .classes


predict_probabilities : Matrix Float -> Model -> Result String (Vector Float)
predict_probabilities xs =
    predict_ xs >> Result.map .probabilities


predict_ : Matrix Float -> Model -> Result String { probabilities : Vector Float, classes : Vector Float }
predict_ xs =
    Result.andThen
        (\{ settings, models } ->
            List.map (predictOneClass settings xs) models
                |> Helpers.flatResultList
                |> Result.map oneVsAllMaxPredictions
        )


predictOneClass : Internal.Settings -> Matrix Float -> ClassModel -> Result String { class : Float, predictions : Vector Float }
predictOneClass settings xs { class, params } =
    let
        model =
            Ok { settings = settings, params = params }
    in
    Internal.predict hypotesis xs model
        |> Result.map (\prediction -> { class = class, predictions = prediction })


hypotesis : Matrix Float -> Vector Float -> Vector Float
hypotesis xs parameters =
    multiplyVector xs parameters
        |> List.map (\z -> 1 / (1 + Math.Constants.e ^ -z))


oneVsAllLabels : Vector Float -> List { class : Float, ys : Vector Float }
oneVsAllLabels ys =
    List.Extra.unique ys
        |> List.map
            (\y ->
                { class = y
                , ys =
                    List.map
                        (\y_ ->
                            if y == y_ then
                                1
                            else
                                0
                        )
                        ys
                }
            )


oneVsAllMaxPredictions : List { class : Float, predictions : Vector Float } -> { classes : Vector Float, probabilities : Vector Float }
oneVsAllMaxPredictions predictions =
    let
        maxPredictions =
            predictions
                |> List.foldl
                    (\{ class, predictions } acc ->
                        acc ++ [ List.map (\prediction -> ( class, prediction )) predictions ]
                    )
                    []
                |> Matrix.transpose
                |> List.map (List.Extra.maximumBy Tuple.second)
                |> List.map (Helpers.unwrapMaybe "Could not find max value in row")
    in
    { classes = List.map Tuple.first maxPredictions
    , probabilities = List.map Tuple.second maxPredictions
    }
