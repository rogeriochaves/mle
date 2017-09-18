module Main exposing (..)

import Array
import Html exposing (..)
import LinearRegression exposing (..)
import Math.Matrix exposing (..)
import Matrix exposing (..)
import Unwrap exposing (..)


data : Matrix Float
data =
    [ [ 1.0, 2.5 ]
    , [ 2.0, 4.5 ]
    , [ 3.0, 6.5 ]
    ]
        |> fromList
        |> unwrap "could not convert list to Matrix"


xs : Matrix Float
xs =
    [ Array.toList (getColumn 0 data |> unwrap "could not get column 0 from data")
    ]
        |> Matrix.fromList
        |> unwrap "could parse to matrix"
        |> transpose


ys : Array.Array Float
ys =
    getColumn 1 data
        |> unwrap "could not get column 1 from data"


main : Html msg
main =
    linearRegression xs ys
        |> toString
        |> text
