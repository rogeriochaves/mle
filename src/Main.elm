module Main exposing (..)

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
    descend data -5
        |> toString
        |> text
