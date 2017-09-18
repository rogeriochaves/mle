module Math.MatrixTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix exposing (..)
import Matrix
import Test exposing (..)


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
                    |> unpack
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
                    |> unpack
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
        ]


sample2x2 : Matrix.Matrix number
sample2x2 =
    Matrix.fromList
        [ [ 1, 2 ]
        , [ 3, 4 ]
        ]
        |> unpack


sample2x3 : Matrix.Matrix number
sample2x3 =
    Matrix.fromList
        [ [ 1, 2, 3 ]
        , [ 4, 5, 6 ]
        ]
        |> unpack



--


unpack : Maybe a -> a
unpack maybe =
    case maybe of
        Just x ->
            x

        _ ->
            Debug.crash ("unpacked failed " ++ toString maybe)
