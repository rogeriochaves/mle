module Mle.NeuralNetwork exposing (..)

import Helpers exposing (..)
import Math.Matrix as Matrix exposing (..)
import Math.Vector as Vector exposing (..)
import Mle.Internal.Regression as Regression
import Mle.LogisticRegression as LogisticRegression


init : Settings -> Model
init settings =
    Ok { params = [], settings = settings, layers = [] }


type alias Model =
    Result String { settings : Settings, params : Vector (Matrix Float), layers : List LayerSettings }


type alias Settings =
    { learningRate : Float, maxIterations : Int }


defaultSettings : Settings
defaultSettings =
    { learningRate = 0.1, maxIterations = 10000 }


type alias LayerSettings =
    { name : String, nodes : Int, initialParams : Matrix Float, activation : String }


defaultLayer : LayerSettings
defaultLayer =
    { name = "unknown", nodes = 1, initialParams = [], activation = "TODO" }


addLayer : String -> LayerSettings -> Model -> Model
addLayer name layer =
    Result.map
        (\model ->
            let
                layer_ =
                    { layer | name = name }

                params =
                    if List.length layer.initialParams > 0 then
                        model.params ++ [ layer.initialParams ]
                    else
                        model.params
            in
            { params = params, settings = model.settings, layers = model.layers ++ [ layer_ ] }
        )


predict : Matrix Float -> Model -> Result String (Matrix Float)
predict xs =
    Result.andThen (forwardPropagation xs)


forwardPropagation : Matrix Float -> { a | params : Vector (Matrix Float) } -> Result String (Matrix Float)
forwardPropagation xs model =
    let
        calculateLayer weights inputs =
            Matrix.multiply (Regression.padFeatures inputs) (Matrix.transpose weights)
                |> Result.map (Matrix.map LogisticRegression.sigmoid)
    in
    List.foldl
        (\weights -> Result.andThen <| calculateLayer weights)
        (Ok xs)
        model.params
