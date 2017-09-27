module LinearRegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import LinearRegression exposing (..)
import Math.Matrix exposing (..)
import Test exposing (..)
import Unwrap exposing (..)


data : Matrix Float
data =
    [ [ 1.0, 3.0 ]
    , [ 2.0, 5.0 ]
    , [ 3.0, 7.0 ]
    ]


xs : Matrix Float
xs =
    getColumns [ 0 ] data
        |> unwrap "could not get xs"
        |> transpose


ys : Vector Float
ys =
    getColumn 1 data
        |> unwrap "could not get ys"


suite : Test
suite =
    describe "LinearRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis xs [ 5 ]) to equal (Ok [ 5, 10, 15 ])
        , describe "param descend"
            [ it "descends a param down for overshooted solution" <|
                expect (paramDescend (padFeatures xs) ys [ 0, 8 ] 1 8) to equal (Ok ( 26, 5.4 ))
            , it "descends a param up for undershooted solution" <|
                expect (paramDescend (padFeatures xs) ys [ 0, -4 ] 1 -4) to equal (Ok ( -30, -1 ))
            , it "keeps params if correct solution" <|
                expect (paramDescend (padFeatures xs) ys [ 1, 2 ] 1 2) to equal (Ok ( 0, 2 ))
            ]
        , describe "gradient descend"
            [ it "descends when guess is wrong" <|
                expect (descend xs ys [ 0.5, 2 ]) to equal (Ok ( -1.5, [ 0.55, 2.1 ] ))
            , it "converges with gradient descend" <|
                expect (gradientDescend xs ys (initialParameters xs) |> Result.map (List.map round)) to equal (Ok [ 1, 2 ])
            , it "keeps same when already converged" <|
                expect (gradientDescend xs ys [ 1, 2 ]) to equal (Ok [ 1, 2 ])
            ]
        , describe "linear regression"
            [ it "trains algorithm" <|
                expect (train xs ys |> Result.map (List.map round)) to equal (Ok [ 1, 2 ])
            , it "predicts future values" <|
                expect (train xs ys |> predict [ [ 4.0 ] ] |> Result.map (List.map round)) to equal (Ok [ 9 ])
            ]
        ]
