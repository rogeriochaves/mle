module Mle.Internal.Regression exposing (..)

import Helpers exposing (..)
import Math.Matrix as Matrix exposing (..)
import NumElm exposing (..)


type alias HypotesisFunction =
    Matrix -> Vector -> Vector


threshold : Float
threshold =
    0.01


padFeatures : Matrix -> Matrix
padFeatures xs =
    NumElm.vector Float64 (List.repeat (Matrix.height xs) 1)
        |> Result.andThen (flip (NumElm.concatAxis 1) xs)
        |> unwrap


descend : HypotesisFunction -> Settings -> Matrix -> Vector -> Vector -> ( Float, Vector )
descend hypotesisFunction settings xs ys parameters =
    let
        xs_ =
            padFeatures xs

        dataSize =
            toFloat (Matrix.height xs_)

        cost =
            NumElm.subtract (hypotesisFunction xs_ parameters) ys
                |> unwrap

        totalCost =
            NumElm.sum cost

        updatedParameters =
            NumElm.dot (Matrix.transpose xs_) cost
                |> unwrap
                |> flip (.*) (settings.learningRate / dataSize)
                |> NumElm.subtract parameters
                |> unwrap
    in
    ( totalCost, updatedParameters )


gradientDescend : HypotesisFunction -> Settings -> Matrix -> Vector -> Vector -> Int -> Result String Vector
gradientDescend hypotesisFunction settings xs ys parameters iteration =
    case descend hypotesisFunction settings xs ys parameters of
        ( error, nextParameters ) ->
            if iteration > settings.maxIterations then
                Err ("Failed to converge at a maximum of " ++ Basics.toString settings.maxIterations ++ " iterations, try scaling the params and adjusting the learn rate")
            else if Basics.abs error < threshold then
                Ok nextParameters
            else
                gradientDescend hypotesisFunction settings xs ys nextParameters (iteration + 1)


type alias Model =
    Result String { settings : Settings, params : Vector }


type alias Settings =
    { learningRate : Float, maxIterations : Int }


defaultSettings : Settings
defaultSettings =
    { learningRate = 0.1, maxIterations = 10000 }


initialParameters : Matrix -> Vector
initialParameters xs =
    NumElm.zeros Float64 [ Matrix.width xs + 1, 1 ]
        |> unwrap


init : Settings -> Model
init settings =
    Ok { params = NumElm.zeros Float64 [ 1 ] |> unwrap, settings = settings }


train : HypotesisFunction -> Matrix -> Vector -> Model -> Model
train hypotesisFunction xs ys =
    Result.andThen
        (\model ->
            gradientDescend hypotesisFunction model.settings xs ys (initialParameters xs) 0
                |> Result.map (\params -> { model | params = params })
        )


predict : HypotesisFunction -> Matrix -> Model -> Result String Vector
predict hypotesisFunction xs =
    Result.map (.params >> hypotesisFunction (padFeatures xs))
