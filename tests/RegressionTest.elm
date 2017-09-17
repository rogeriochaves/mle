module RegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Regression exposing (..)
import Test exposing (..)


data : List ( Float, Float )
data =
    [ ( 1.0, 1.0 )
    , ( 2.0, 2.0 )
    , ( 3.0, 3.0 )
    ]


suite : Test
suite =
    describe "Regression"
        [ it "descends when guess is wrong" <|
            expect (descend data 10) to equal 5.8
        , it "converges with gradient descend" <|
            expect (gradientDescend data 10 |> round) to equal 1
        , it "keeps same when already converged" <|
            expect (gradientDescend data 1) to equal 1
        ]
