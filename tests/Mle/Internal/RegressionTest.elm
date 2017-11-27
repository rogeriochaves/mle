module Mle.Internal.RegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Helpers exposing (unwrap)
import Math.Matrix exposing (..)
import Mle.Internal.Regression exposing (..)
import NumElm exposing (Dtype(Float64), matrix, vector)
import Test exposing (..)


data : Matrix
data =
    mat
        [ [ 1.0, 3.0 ]
        , [ 2.0, 5.0 ]
        , [ 3.0, 7.0 ]
        ]


xs : Matrix
xs =
    unsafeGetColumns [ 0 ] data


ys : Vector
ys =
    unsafeGetColumn 1 data


hypotesis : HypotesisFunction
hypotesis xs parameters =
    NumElm.dot xs parameters
        |> unwrap


suite : Test
suite =
    describe "LinearRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis xs (vec [ 5 ])) to equal (vec [ 5, 10, 15 ])
        , describe "gradient descend"
            [ it "descends when guess is wrong" <|
                expect (descend hypotesis defaultSettings xs ys (vec [ 0.5, 2 ])) to equal ( -1.5, vec [ 0.55, 2.1 ] )
            , it "converges with gradient descend" <|
                expect (gradientDescend hypotesis defaultSettings xs ys (initialParameters xs) 0 |> Result.map NumElm.round) to equal (Ok <| vec [ 1, 2 ])
            , it "keeps same when already converged" <|
                expect (gradientDescend hypotesis defaultSettings xs ys (vec [ 1, 2 ]) 0) to equal (Ok <| vec [ 1, 2 ])
            ]
        , describe "linear regression"
            [ it "trains algorithm" <|
                expect (init defaultSettings |> train hypotesis xs ys |> Result.map (.params >> NumElm.round)) to equal (Ok <| vec [ 1, 2 ])
            , it "predicts future values" <|
                expect (init defaultSettings |> train hypotesis xs ys |> predict hypotesis (mat [ [ 4.0 ] ]) |> Result.map NumElm.round) to equal (Ok <| vec [ 9 ])
            , it "limits attempts to converge" <|
                expect (init { defaultSettings | maxIterations = 2 } |> train hypotesis xs ys) to equal (Err "Failed to converge at a maximum of 2 iterations, try scaling the params and adjusting the learn rate")
            ]
        ]
