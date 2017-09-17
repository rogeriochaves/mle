module SquaredErrorTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.DualNumber exposing (toDual)
import SquaredError exposing (..)
import Test exposing (..)


data : List ( Float, Float )
data =
    [ ( 1.0, 1.0 )
    , ( 2.0, 2.0 )
    , ( 3.0, 3.0 )
    ]


suite : Test
suite =
    describe "SquaredError"
        [ it "returns the correct squared error for correct solution" <|
            expect (squaredError data (toDual 1)) to equal (toDual 0)
        , it "returns the correct squared error for overshooted solution" <|
            expect (squaredError data (toDual 4)) to equal (toDual 21)
        , it "returns the correct squared error for undershooted solution" <|
            expect (squaredError data (toDual -5)) to equal (toDual 84)
        ]
