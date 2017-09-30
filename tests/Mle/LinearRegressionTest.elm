module Mle.LinearRegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Mle.LinearRegression exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "LinearRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis [ [ 1.0 ], [ 2.0 ], [ 3.0 ] ] [ 5 ]) to equal [ 5, 10, 15 ]
        ]
