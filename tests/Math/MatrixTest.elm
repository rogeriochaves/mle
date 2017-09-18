module Math.MatrixTest exposing (..)

import Array
import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix exposing (..)
import Matrix
import Test exposing (..)
import Unwrap exposing (..)


-- You can validate the matrix operations using Wolfram Alpha, example:
-- http://www.wolframalpha.com/input/?i=%5B%5B1;2%5D,%5B3;4%5D%5D*%5B%5B1,2,3%5D,%5B4,5,6%5D%5D


suite : Test
suite =
    describe "Math.Matrix"
        [ it "foldl a matrix" <|
            expect (foldl (+) 0 sample2x3) to equal 21
        , it "foldl with index" <|
            expect (indexedFoldl (\i j _ total -> total + i + j) 0 sample2x3) to equal 9
        , it "transposes square matrixes" <|
            expect (transpose sample2x2)
                to
                equal
                (Matrix.fromList
                    [ [ 1, 3 ]
                    , [ 2, 4 ]
                    ]
                    |> unwrap "could not convert list to Matrix"
                )
        , it "transposes non-squared matrixes" <|
            expect (transpose sample2x3)
                to
                equal
                (Matrix.fromList
                    [ [ 1, 4 ]
                    , [ 2, 5 ]
                    , [ 3, 6 ]
                    ]
                    |> unwrap "could not convert list to Matrix"
                )
        , it "multiplies matrixes 2x2 and 2x3" <|
            expect (multiply sample2x2 sample2x3)
                to
                equal
                (Matrix.fromList
                    [ [ 9, 12, 15 ]
                    , [ 19, 26, 33 ]
                    ]
                )
        , it "multiplies matrixes 2x3 and 3x2" <|
            expect (multiply sample2x3 (transpose sample2x3))
                to
                equal
                (Matrix.fromList
                    [ [ 14, 32 ]
                    , [ 32, 77 ]
                    ]
                )
        , it "adds a column" <|
            expect (addColumn (Array.fromList [ 0, 0 ]) sample2x3)
                to
                equal
                (Matrix.fromList
                    [ [ 0, 1, 2, 3 ]
                    , [ 0, 4, 5, 6 ]
                    ]
                )
        ]


sample2x2 : Matrix.Matrix number
sample2x2 =
    Matrix.fromList
        [ [ 1, 2 ]
        , [ 3, 4 ]
        ]
        |> unwrap "could not convert list to Matrix"


sample2x3 : Matrix.Matrix number
sample2x3 =
    Matrix.fromList
        [ [ 1, 2, 3 ]
        , [ 4, 5, 6 ]
        ]
        |> unwrap "could not convert list to Matrix"
