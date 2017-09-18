module LinearRegression exposing (..)

import Array exposing (Array)
import Array.Extra
import Math.Matrix exposing (..)
import Matrix exposing (..)
import Unwrap exposing (..)


learningRate : Float
learningRate =
    0.1


threshold : Float
threshold =
    0.001


hypotesis : Matrix Float -> Array Float -> Array Float
hypotesis xs parameters =
    let
        parameters_ =
            Matrix.fromList [ Array.toList parameters ]
                |> unwrap "could not convert parameters list to Matrix"
                |> transpose
    in
    multiply xs parameters_
        |> unwrap "could not multiply matrixes"
        |> getColumn 0
        |> unwrap "could not get row 0"


paramDescend : Matrix Float -> Array Float -> Array Float -> Int -> Float -> ( Float, Float )
paramDescend xs ys parameters index param =
    let
        dataSize =
            toFloat (Matrix.height xs)
    in
    hypotesis xs parameters
        |> Array.Extra.map2 (\y h -> h - y) ys
        |> Array.Extra.map2 (*) (Matrix.getColumn index xs |> unwrap "could not get column")
        |> Array.foldl (+) 0
        |> (\sumHypotesis -> (1 / dataSize) * sumHypotesis)
        |> (\squaredError -> ( squaredError, param - learningRate * squaredError ))


padFeatures : Matrix Float -> Matrix Float
padFeatures xs =
    xs
        |> addColumn (Array.repeat (Matrix.height xs) 1)
        |> unwrap "could not concat 1s to features list"


descend : Matrix Float -> Array Float -> Array Float -> ( Float, Array Float )
descend xs ys parameters =
    let
        xs_ =
            padFeatures xs

        updatedParamsAndErrors =
            Array.indexedMap (paramDescend xs_ ys parameters) parameters

        totalErrors =
            Array.foldl (\( error, _ ) total -> total + error) 0 updatedParamsAndErrors

        updatedParams =
            Array.map Tuple.second updatedParamsAndErrors
    in
    ( totalErrors, updatedParams )


gradientDescend : Matrix Float -> Array Float -> Array Float -> Array Float
gradientDescend xs ys parameters =
    let
        ( error, nextParameters ) =
            descend xs ys parameters
    in
    if abs error < threshold then
        nextParameters
    else
        gradientDescend xs ys nextParameters


initialParameters : Matrix a -> Array number
initialParameters xs =
    Array.repeat (Matrix.width xs + 1) 0


linearRegression : Matrix Float -> Array Float -> Array Float
linearRegression xs ys =
    gradientDescend xs ys (initialParameters xs)
