module Mle.NeuralNetworkTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Mle.NeuralNetwork as NeuralNetwork exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "NeuralNetwork"
        [ it "builds a neural network for an OR function" <|
            let
                model =
                    NeuralNetwork.init { defaultSettings | learningRate = 1 }
                        |> addLayer "input layer"
                            { defaultLayer
                                | nodes = 2
                                , initialParams = [ [ -10, 20, 20 ] ]
                            }
                        |> addLayer "output layer"
                            { defaultLayer
                                | nodes = 1
                                , activation = "TODO (will be logistic for now)"
                            }

                testXs =
                    [ [ 1, 1 ]
                    , [ 0, 0 ]
                    , [ 1, 0 ]
                    , [ 0, 1 ]
                    ]
            in
            expect (NeuralNetwork.predict testXs model) to equal (Ok [ 1, 0, 1, 1 ])
        ]
