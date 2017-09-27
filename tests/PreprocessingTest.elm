module PreprocessingTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import List.Extra
import Math.Matrix exposing (..)
import Preprocessing exposing (..)
import Random
import Random.List
import Test exposing (..)
import Unwrap exposing (unwrap)


xsBig : Matrix Float
xsBig =
    [ [ 10, 30 ]
    , [ 20, 70 ]
    , [ 30, 50 ]
    ]


xsSmall : Matrix Float
xsSmall =
    [ [ 0.001, 0.003 ]
    , [ 0.002, 0.007 ]
    , [ 0.003, 0.005 ]
    ]


suite : Test
suite =
    describe "Preprocessing"
        [ describe "scaling" <|
            [ it "scales a matrix down" <|
                expect (scaleMatrix xsBig)
                    to
                    equal
                    [ [ -0.5, -0.5 ]
                    , [ 0.0, 0.5 ]
                    , [ 0.5, 0.0 ]
                    ]
            , it "scales a matrix up" <|
                expect (scaleMatrix xsSmall)
                    to
                    equal
                    [ [ -0.5, -0.5 ]
                    , [ 0.0, 0.5 ]
                    , [ 0.5, 0.0 ]
                    ]
            , it "scales a vector" <|
                expect (getColumn 0 xsBig |> unwrap "" |> scaleVector) to equal [ -0.5, 0.0, 0.5 ]
            , fuzz2 (fuzzVector 3) (Fuzz.intRange 0 2) "always keep values between -1 and +1 on vectors" <|
                \fuzzVector index ->
                    scaleVector fuzzVector
                        |> List.Extra.getAt index
                        |> Maybe.withDefault 999
                        |> Expect.all
                            [ greaterThan -1
                            , atMost 1
                            ]
            , fuzz3 (fuzzMatrix 3 4) (Fuzz.intRange 0 2) (Fuzz.intRange 0 3) "always keep values between -1 and +1 on matrixes" <|
                \fuzzMatrix i j ->
                    scaleMatrix fuzzMatrix
                        |> Math.Matrix.getAt i j
                        |> Maybe.withDefault 999
                        |> Expect.all
                            [ greaterThan -1
                            , atMost 1
                            ]
            ]
        , describe "trainTestSplit"
            [ it "splits train and test data" <|
                let
                    xs =
                        [ List.range 0 10 ] |> transpose

                    ys =
                        List.range 10 20

                    seed =
                        Random.initialSeed 1

                    trainXs =
                        [ [ 10 ], [ 4 ], [ 8 ], [ 7 ], [ 6 ], [ 5 ], [ 0 ], [ 2 ], [ 1 ] ]

                    trainYs =
                        [ 20, 14, 18, 17, 16, 15, 10, 12, 11 ]

                    testXs =
                        [ [ 9 ], [ 3 ] ]

                    testYs =
                        [ 19, 13 ]

                    ( _, nextSeed ) =
                        Random.step (Random.List.shuffle ys) seed
                in
                expect (trainTestSplit xs ys seed) to equal ( trainXs, trainYs, testXs, testYs, nextSeed )
            ]
        ]


fuzzMatrix : Int -> Int -> Fuzzer (Matrix Float)
fuzzMatrix rows columns =
    List.repeat rows (fuzzVector columns)
        |> fuzzerTranspose


fuzzVector : Int -> Fuzzer (Vector Float)
fuzzVector length =
    List.repeat length Fuzz.float
        |> fuzzerTranspose


fuzzerTranspose : List (Fuzzer a) -> Fuzzer (List a)
fuzzerTranspose =
    List.foldr (Fuzz.map2 (::)) (Fuzz.constant [])
