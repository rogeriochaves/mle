module Mle.LinearRegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix exposing (..)
import Math.Vector exposing (..)
import Mle.LinearRegression exposing (..)
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


suite : Test
suite =
    describe "LinearRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis xs [ 5 ]) to equal [ 5, 10, 15 ]
        , describe "gradient descend"
            [ it "descends when guess is wrong" <|
                expect (descend defaultSettings xs ys [ 0.5, 2 ]) to equal ( -1.5, [ 0.55, 2.1 ] )
            , it "converges with gradient descend" <|
                expect (gradientDescend defaultSettings xs ys (initialParameters xs) 0 |> Result.map (List.map round)) to equal (Ok [ 1, 2 ])
            , it "keeps same when already converged" <|
                expect (gradientDescend defaultSettings xs ys [ 1, 2 ] 0) to equal (Ok [ 1, 2 ])
            ]
        , describe "linear regression"
            [ it "trains algorithm" <|
                expect (init defaultSettings |> train xs ys |> Result.map (.params >> List.map round)) to equal (Ok [ 1, 2 ])
            , it "predicts future values" <|
                expect (init defaultSettings |> train xs ys |> predict [ [ 4.0 ] ] |> Result.map (List.map round)) to equal (Ok [ 9 ])
            , it "limits attempts to converge" <|
                expect (init { defaultSettings | maxIterations = 2 } |> train xs ys) to equal (Err "Failed to converge at a maximum of 2 iterations, try scaling the params and adjusting the learn rate")
            ]
        ]
