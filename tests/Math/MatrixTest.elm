module Math.MatrixTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.Matrix exposing (..)
import Test exposing (..)


-- You can validate the matrix operations using Wolfram Alpha, example:
-- http://www.wolframalpha.com/input/?i=%5B%5B1;2%5D,%5B3;4%5D%5D*%5B%5B1,2,3%5D,%5B4,5,6%5D%5D


suite : Test
suite =
    describe "Math.Matrix"
        [ it "transposes square matrixes" <|
            expect (transpose sample2x2)
                to
                equal
                [ [ 1, 3 ]
                , [ 2, 4 ]
                ]
        , it "transposes non-squared matrixes" <|
            expect (transpose sample2x3)
                to
                equal
                [ [ 1, 4 ]
                , [ 2, 5 ]
                , [ 3, 6 ]
                ]
        , it "multiplies matrixes 2x2 and 2x3" <|
            expect (multiply sample2x2 sample2x3)
                to
                equal
                (Ok
                    [ [ 9, 12, 15 ]
                    , [ 19, 26, 33 ]
                    ]
                )
        , it "multiplies matrixes 2x3 and 3x2" <|
            expect (multiply sample2x3 (transpose sample2x3))
                to
                equal
                (Ok
                    [ [ 14, 32 ]
                    , [ 32, 77 ]
                    ]
                )
        , it "fails to multiply 2x3 and 2x3 matrixes" <|
            expect (multiply sample2x3 sample2x3)
                to
                equal
                (Err "Matrix sizes are not fit for multiplication: Could not multiply a (2,3) matrix with a (2,3) matrix")
        , it "prepends a column" <|
            expect (prependColumn [ 0, 0 ] sample2x3)
                to
                equal
                (Ok
                    [ [ 0, 1, 2, 3 ]
                    , [ 0, 4, 5, 6 ]
                    ]
                )
        , it "does prepends a column with different size" <|
            expect (prependColumn [ 0, 0, 0 ] sample2x3)
                to
                equal
                (Err "Could not prepend a 3-sized vector to a (2,3) matrix")
        , it "returns correct width" <|
            expect (width sample2x3) to equal 3
        , it "returns correct height" <|
            expect (height sample2x3) to equal 2
        , it "returns correct size" <|
            expect (size sample2x3) to equal ( 2, 3 )
        , it "gets a row" <|
            expect (getRow 1 sample2x3) to equal (Just [ 4, 5, 6 ])
        ]


sample2x2 : Matrix number
sample2x2 =
    [ [ 1, 2 ]
    , [ 3, 4 ]
    ]


sample2x3 : Matrix number
sample2x3 =
    [ [ 1, 2, 3 ]
    , [ 4, 5, 6 ]
    ]
