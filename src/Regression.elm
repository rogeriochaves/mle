module Regression exposing (..)

import DualNumber exposing (..)
import SquaredError exposing (squaredError)


learningRate : Float
learningRate =
    0.1


descend : List ( Float, Float ) -> Float -> Float
descend data parameter1 =
    parameter1 - (learningRate * derivative (squaredError data) parameter1)
