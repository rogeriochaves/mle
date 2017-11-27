module Mle.LogisticRegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Fuzz
import Mle.LogisticRegression exposing (..)
import NumElm
import Test exposing (..)
import TestHelpers exposing (..)


suite : Test
suite =
    describe "LogisticRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis (mat [ [ 0.5, 0.3 ], [ 0.2, 0.01 ] ]) (vec [ 1, 5 ]))
                to
                equal
                (vec [ 0.8807970779778825, 0.5621765008857982 ])
        , fuzz3 (fuzzMatrix 5 3) (fuzzVector 3) (Fuzz.intRange 0 2) "always keep hypotesis values between 0 and 1" <|
            \xs params i ->
                hypotesis xs params
                    |> NumElm.get [ i, 0 ]
                    |> Maybe.withDefault 999
                    |> Expect.all
                        [ atLeast 0
                        , atMost 1
                        ]
        ]
