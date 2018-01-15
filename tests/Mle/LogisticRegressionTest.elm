module Mle.LogisticRegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Fuzz
import List.Extra
import Mle.Internal.Regression as Internal
import Mle.LogisticRegression exposing (..)
import Test exposing (..)
import TestHelpers exposing (..)


suite : Test
suite =
    describe "LogisticRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis [ [ 0.5, 0.3 ], [ 0.2, 0.01 ] ] [ 1, 5 ])
                to
                equal
                [ 0.8807970779778825, 0.5621765008857982 ]
        , fuzz3 (fuzzMatrix 5 3) (fuzzVector 5) (Fuzz.intRange 0 4) "always keep hypotesis values between 0 and 1" <|
            \xs params i ->
                hypotesis xs params
                    |> List.Extra.getAt i
                    |> Maybe.withDefault 999
                    |> Expect.all
                        [ atLeast 0
                        , atMost 1
                        ]
        , it "generates one-vs-all labels for each different class" <|
            expect (oneVsAllLabels [ 1, 1, 2, 1, 3, 3 ])
                to
                equal
                [ { class = 1, ys = [ 1, 1, 0, 1, 0, 0 ] }
                , { class = 2, ys = [ 0, 0, 1, 0, 0, 0 ] }
                , { class = 3, ys = [ 0, 0, 0, 0, 1, 1 ] }
                ]
        , it "trains a single class" <|
            let
                xs =
                    [ [ 1 ], [ 100 ] ]

                ys =
                    [ 1, 0 ]
            in
            expect (trainOneClass defaultSettings xs { class = 5, ys = ys })
                to
                equal
                (Ok { class = 5, params = [ 0, -2.475 ] })
        , it "creates one model for each different class" <|
            expect
                (init defaultSettings
                    |> train [ [ 1 ], [ 10 ], [ 100 ] ] [ 1, 2, 3 ]
                )
                to
                equal
                (Ok
                    { settings = { learningRate = 0.1, maxIterations = 10000 }
                    , models =
                        [ { class = 1, params = [ 2.7735236079360304, -0.7102820408987571 ] }
                        , { class = 2, params = [ 1.0270584575201647, 0.008036015198871982 ] }
                        , { class = 3, params = [ -2.085783294801828, 0.8993001339955669 ] }
                        ]
                    }
                )
        , it "predicts a single class" <|
            let
                xs =
                    [ [ 1 ], [ 100 ] ]

                ys =
                    [ 1, 0 ]
            in
            expect (predictOneClass defaultSettings xs { class = 5, params = [ -10, 10 ] })
                to
                equal
                (Ok { class = 5, predictions = [ 0.5, 1 ] })
        , it "flattens a list of one-vs-all predictions" <|
            let
                predictions =
                    [ { class = 5, predictions = [ 0.8, 0 ] }
                    , { class = 7, predictions = [ 0.9, 0.2 ] }
                    , { class = 9, predictions = [ 0.3, 0.4 ] }
                    ]
            in
            expect (oneVsAllMaxPredictions predictions)
                to
                equal
                { classes = [ 7, 9 ]
                , probabilities = [ 0.9, 0.4 ]
                }
        , it "predicts classes correctly" <|
            expect
                (init defaultSettings
                    |> train [ [ 0 ], [ 5 ], [ 10 ] ] [ 7, 8, 9 ]
                    |> predict [ [ 1 ], [ 6 ] ]
                )
                to
                equal
                (Ok [ 7, 8 ])
        , it "predicts probabilities correctly" <|
            expect
                (init defaultSettings
                    |> train [ [ 0 ], [ 5 ], [ 10 ] ] [ 7, 8, 9 ]
                    |> predict_probabilities [ [ 1 ], [ 6 ] ]
                )
                to
                equal
                (Ok [ 0.671798244339312, 0.3551530955072557 ])
        ]
