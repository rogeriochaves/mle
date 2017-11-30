module Math.MatrixTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Helpers exposing (unwrap)
import Math.Matrix exposing (..)
import NumElm exposing (Dtype(Float64), NdArray, matrix, vector)
import Test exposing (..)


-- You can validate the matrix operations using Wolfram Alpha, example:
-- http://www.wolframalpha.com/input/?i=%5B%5B1;2%5D,%5B3;4%5D%5D*%5B%5B1,2,3%5D,%5B4,5,6%5D%5D


suite : Test
suite =
    describe "Math.Matrix"
        [ it "converts a matrix to list" <|
            expect (sample2x2 |> toList)
                to
                equal
                (Ok
                    [ [ 1, 2 ]
                    , [ 3, 4 ]
                    ]
                )
        , it "converts a vector to list" <|
            expect (sample2x2 |> getColumn 0 |> Helpers.unwrapMaybe "should not happen" |> vectorToList)
                to
                equal
                (Ok [ 1, 3 ])
        , it "transposes square matrixes" <|
            expect (transpose sample2x2 |> Ok)
                to
                equal
                (matrix Float64
                    [ [ 1, 3 ]
                    , [ 2, 4 ]
                    ]
                )
        , it "transposes non-squared matrixes" <|
            expect (transpose sample2x3 |> Ok)
                to
                equal
                (matrix Float64
                    [ [ 1, 4 ]
                    , [ 2, 5 ]
                    , [ 3, 6 ]
                    ]
                )
        , it "returns correct width" <|
            expect (width sample2x3) to equal 3
        , it "returns correct height" <|
            expect (height sample2x3) to equal 2
        , it "get a column" <|
            expect (getColumn 1 sample2x3)
                to
                equal
                (vector Float64 [ 2, 5 ] |> Result.toMaybe)
        , it "get two columns" <|
            expect (getColumns [ 0, 2 ] sample2x3)
                to
                equal
                (matrix Float64
                    [ [ 1, 3 ]
                    , [ 4, 6 ]
                    ]
                    |> Result.toMaybe
                )
        ]


sample2x2 : NdArray
sample2x2 =
    matrix Float64
        [ [ 1, 2 ]
        , [ 3, 4 ]
        ]
        |> unwrap


sample2x3 : NdArray
sample2x3 =
    matrix Float64
        [ [ 1, 2, 3 ]
        , [ 4, 5, 6 ]
        ]
        |> unwrap
