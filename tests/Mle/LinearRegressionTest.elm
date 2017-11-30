module Mle.LinearRegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix exposing (..)
import Mle.LinearRegression exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "LinearRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis (mat [ [ 1.0 ], [ 2.0 ], [ 3.0 ] ]) (vec [ 5 ]))
                to
                equal
                (vec [ 5, 10, 15 ])
        ]
