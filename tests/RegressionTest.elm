module RegressionTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Regression exposing (..)
import Test exposing (..)


data : List ( Float, Float )
data =
    [ ( 1.0, 1.0 )
    , ( 2.0, 2.0 )
    , ( 3.0, 3.0 )
    ]


suite : Test
suite =
    describe "Regression"
        [ it "descends when guess is wrong" <|
            expect (descend data 10) to equal 5.8
        , it "converges after descending a few times" <|
            let
                result =
                    descend data 10
                        |> descend data
                        |> descend data
                        |> descend data
                        |> descend data
                        |> descend data
                        |> descend data
                        |> descend data
            in
            result
                |> Expect.all [ greaterThan 1, atMost 2 ]
        ]
