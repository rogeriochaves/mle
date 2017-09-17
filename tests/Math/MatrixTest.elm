module Math.MatrixTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix exposing (..)
import Matrix
import Test exposing (..)


-- You can validate all derivatives using Wolfram Alpha, example:
-- http://www.wolframalpha.com/input/?i=d%2Fdx+f(x)%3D3%2Bx-(x%5E2%2F2x),x%3D72


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
