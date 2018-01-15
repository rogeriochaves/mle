module Mle.Internal.RegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix exposing (..)
import Math.Vector exposing (..)
import Mle.Internal.Regression exposing (..)
import Test exposing (..)


data : Matrix Float
data =
    [ [ 1.0, 3.0 ]
    , [ 2.0, 5.0 ]
    , [ 3.0, 7.0 ]
    ]


xs : Matrix Float
xs =
    unsafeGetColumns [ 0 ] data


ys : Vector Float
ys =
    unsafeGetColumn 1 data


hypotesis : HypotesisFunction
hypotesis xs parameters =
    multiplyVector xs parameters


suite : Test
suite =
    describe "Regression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis xs [ 5 ]) to equal [ 5, 10, 15 ]
        , describe "gradient descend"
            [ it "descends when guess is wrong" <|
                expect (descend hypotesis defaultSettings xs ys [ 0.5, 2 ]) to equal ( -1.5, [ 0.55, 2.1 ] )
            , it "converges with gradient descend" <|
                expect (gradientDescend hypotesis defaultSettings xs ys (initialParameters xs) 0 |> Result.map (List.map round)) to equal (Ok [ 1, 2 ])
            , it "keeps same when already converged" <|
                expect (gradientDescend hypotesis defaultSettings xs ys [ 1, 2 ] 0) to equal (Ok [ 1, 2 ])
            ]
        , describe "regression"
            [ it "trains algorithm" <|
                expect (init defaultSettings |> train hypotesis xs ys |> Result.map (.params >> List.map round)) to equal (Ok [ 1, 2 ])
            , it "predicts future values" <|
                expect (init defaultSettings |> train hypotesis xs ys |> predict hypotesis [ [ 4.0 ] ] |> Result.map (List.map round)) to equal (Ok [ 9 ])
            , it "exits as is when it reaches the max number of interactions before converging" <|
                expect (init { defaultSettings | maxIterations = 2 } |> train hypotesis xs ys)
                    to
                    equal
                    (Ok
                        { params = [ 0.8686703703703704, 1.96179012345679 ]
                        , settings = { learningRate = 0.1, maxIterations = 2 }
                        }
                    )
            ]
        ]
