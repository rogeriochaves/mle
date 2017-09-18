module LinearRegressionTest exposing (..)

import Array
import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import LinearRegression exposing (..)
import Math.Matrix exposing (..)
import Matrix exposing (..)
import Test exposing (..)
import Unwrap exposing (..)


data : Matrix Float
data =
    [ [ 1.0, 3.0 ]
    , [ 2.0, 5.0 ]
    , [ 3.0, 7.0 ]
    ]
        |> fromList
        |> unwrap "could not convert list to Matrix"


xs : Matrix Float
xs =
    [ Array.toList (getColumn 0 data |> unwrap "could not get column 0 from data")
    ]
        |> Matrix.fromList
        |> unwrap "could parse to matrix"
        |> transpose


ys : Array.Array Float
ys =
    getColumn 1 data
        |> unwrap "could not get column 1 from data"


suite : Test
suite =
    describe "LinearRegression"
        [ it "calculates the hypotesis for all parameters" <|
            expect (hypotesis xs (Array.fromList [ 5 ])) to equal (Array.fromList [ 5, 10, 15 ])
        , it "descends a param down for overshooted solution" <|
            expect (paramDescend (padFeatures xs) ys (Array.fromList [ 0, 8 ]) 1 8) to equal ( 26, 5.4 )
        , it "descends a param up for undershooted solution" <|
            expect (paramDescend (padFeatures xs) ys (Array.fromList [ 0, -4 ]) 1 -4) to equal ( -30, -1 )
        , it "keeps params if correct solution" <|
            expect (paramDescend (padFeatures xs) ys (Array.fromList [ 1, 2 ]) 1 2) to equal ( 0, 2 )
        , it "descends when guess is wrong" <|
            expect (descend xs ys (Array.fromList [ 0.5, 2 ])) to equal ( -1.5, Array.fromList [ 0.55, 2.1 ] )
        , it "converges with gradient descend" <|
            expect (gradientDescend xs ys (initialParameters xs) |> Array.map round) to equal (Array.fromList [ 1, 2 ])
        , it "keeps same when already converged" <|
            expect (gradientDescend xs ys (Array.fromList [ 1, 2 ])) to equal (Array.fromList [ 1, 2 ])
        , it "applies linear regression to data" <|
            expect (linearRegression xs ys |> Array.map round) to equal (Array.fromList [ 1, 2 ])
        ]
