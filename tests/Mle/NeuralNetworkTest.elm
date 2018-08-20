module Mle.NeuralNetworkTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix as Matrix
import Mle.NeuralNetwork as NeuralNetwork exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "NeuralNetwork"
        [ it "forward propagates" <|
            let
                weights =
                    [ [ -- Layer 1
                        [ -10, 20, 20 ] -- Layer 1 weights for connection with Layer 2 node 1, include padding weight
                      ]
                    ]

                inputs =
                    [ [ 0, 1 ] ]

                result =
                    forwardPropagation inputs { params = weights }
                        |> Result.map (Matrix.map round)
            in
            expect result to equal (Ok [ [ 1 ] ])
        , it "builds a neural network for an OR function" <|
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

                result =
                    NeuralNetwork.predict testXs model
                        |> Result.map (Matrix.map round)
            in
            expect result to equal (Ok [ [ 1 ], [ 0 ], [ 1 ], [ 1 ] ])
        , it "builds a neural network for an AND function" <|
            let
                model =
                    NeuralNetwork.init { defaultSettings | learningRate = 1 }
                        |> addLayer "input layer"
                            { defaultLayer
                                | nodes = 2
                                , initialParams = [ [ -30, 20, 20 ] ]
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

                result =
                    NeuralNetwork.predict testXs model
                        |> Result.map (Matrix.map round)
            in
            expect result to equal (Ok [ [ 1 ], [ 0 ], [ 0 ], [ 0 ] ])
        , it "builds a neural network for an XNOR function" <|
            let
                model =
                    NeuralNetwork.init { defaultSettings | learningRate = 1 }
                        |> addLayer "input layer"
                            { defaultLayer
                                | nodes = 2
                                , initialParams =
                                    [ [ -30, 20, 20 ]
                                    , [ 10, -20, -20 ]
                                    ]
                            }
                        |> addLayer "hidden layer"
                            { defaultLayer
                                | nodes = 2
                                , initialParams =
                                    [ [ -10, 20, 20 ]
                                    ]
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

                result =
                    NeuralNetwork.predict testXs model
                        |> Result.map (Matrix.map round)
            in
            expect result to equal (Ok [ [ 1 ], [ 1 ], [ 0 ], [ 0 ] ])
        ]
