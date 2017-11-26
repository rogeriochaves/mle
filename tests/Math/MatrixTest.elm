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
        , it "multiplies matrixes 2x2 and 2x3" <|
            expect (multiply sample2x2 sample2x3)
                to
                equal
                (matrix Float64
                    [ [ 9, 12, 15 ]
                    , [ 19, 26, 33 ]
                    ]
                )
        , it "multiplies matrixes 2x3 and 3x2" <|
            expect (multiply sample2x3 (transpose sample2x3))
                to
                equal
                (matrix Float64
                    [ [ 14, 32 ]
                    , [ 32, 77 ]
                    ]
                )
        , it "fails to multiply 2x3 and 2x3 matrixes" <|
            expect (multiply sample2x3 sample2x3)
                to
                equal
                (Err "NdArray#dot - Incompatible shapes: The shape of nda1 is 2×3, but nda2 says 2×3")
        , it "prepends a column" <|
            expect (prependColumn (vector Float64 [ 0, 0 ] |> unwrap) sample2x3)
                to
                equal
                (matrix Float64
                    [ [ 0, 1, 2, 3 ]
                    , [ 0, 4, 5, 6 ]
                    ]
                )
        , it "does prepends a column with different size" <|
            expect (prependColumn (vector Float64 [ 0, 0, 0 ] |> unwrap) sample2x3)
                to
                equal
                (Err "NdArray#concat - Incompatible shapes: The shape of nda1 is 3×1, but nda2 says 2×3 on axis 1")
        , it "returns correct width" <|
            expect (width sample2x3) to equal 3
        , it "returns correct height" <|
            expect (height sample2x3) to equal 2
        , it "gets a value" <|
            expect (getAt 1 2 sample2x3) to equal (Just 6)
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
        , it "multiplies by a vector" <|
            expect (multiplyVector sample2x3 (NumElm.vector Float64 [ 2, 3, 5 ] |> unwrap))
                to
                equal
                (vector Float64 [ 23, 53 ])
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
