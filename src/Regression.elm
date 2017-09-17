module Regression exposing (..)

import DualNumber exposing (..)
import SquaredError exposing (squaredError)


learningRate : Float
learningRate =
    0.1


threshold : Float
threshold =
    0.01


descend : List ( Float, Float ) -> Float -> Float
descend data parameter1 =
    parameter1 - (learningRate * derivative (squaredError data) parameter1)


gradientDescend : List ( Float, Float ) -> Float -> Float
gradientDescend data parameter1 =
    let
        nextParameter1 =
            descend data parameter1
    in
    if abs (parameter1 - nextParameter1) < threshold then
        nextParameter1
    else
        gradientDescend data nextParameter1
