module Main exposing (..)

import DualNumber exposing (derivative)
import Html exposing (..)
import Regression exposing (..)


data : List ( Float, Float )
data =
    [ ( 1.0, 1.0 )
    , ( 2.0, 2.0 )
    , ( 3.0, 3.0 )
    ]


main : Html msg
main =
    derivative (squaredError data) 1
        |> toString
        |> text
