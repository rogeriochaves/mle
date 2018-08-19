module Mle.NeuralNetwork exposing (..)

import Helpers exposing (..)
import Math.Matrix as Matrix exposing (..)
import Math.Vector as Vector exposing (..)
import Mle.Internal.Regression as Regression


init : Settings -> Model
init settings =
    Ok { params = [], settings = settings, layers = [] }


type alias Model =
    Result String { settings : Settings, params : Matrix Float, layers : List LayerSettings }


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
            in
            { params = [], settings = model.settings, layers = model.layers ++ [ layer_ ] }
        )


predict : Matrix Float -> Model -> Result String (Vector Float)
predict xs model =
    Err "not implemented"
