module Main exposing (..)

import Html exposing (..)
import LinearRegression exposing (..)
import Math.Matrix exposing (..)
import Unwrap exposing (..)


data : Matrix Float
data =
    [ [ 1.0, 2.5 ]
    , [ 2.0, 4.5 ]
    , [ 3.0, 6.5 ]
    ]


xs : Matrix Float
xs =
    getColumns [ 0 ] data
        |> unwrap "could not get xs"
        |> transpose


ys : Vector Float
ys =
    getColumn 1 data
        |> unwrap "could not get ys"


main : Html msg
main =
    linearRegression xs ys
        |> toString
        |> text
