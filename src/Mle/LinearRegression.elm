module Mle.LinearRegression exposing (..)

import Helpers exposing (..)
import Math.Matrix as Matrix exposing (..)


learningRate : Float
learningRate =
    0.1


threshold : Float
threshold =
    0.01


hypotesis : Matrix Float -> Vector Float -> Result String (Vector Float)
hypotesis xs parameters =
    let
        parameters_ =
            transpose [ parameters ]
    in
    multiply xs parameters_
        |> Result.andThen
            (getColumn 0 >> Result.fromMaybe "could not find column 0")


paramDescend : Matrix Float -> Vector Float -> Vector Float -> Int -> Float -> Result String ( Float, Float )
paramDescend xs ys parameters index param =
    let
        dataSize =
            toFloat (Matrix.height xs)
    in
    hypotesis xs parameters
        |> Result.map
            (List.map2 (\y h -> h - y) ys
                >> List.map2 (*) (Matrix.getColumn index xs |> unwrapMaybe "could not get column")
                >> List.foldl (+) 0
                >> (\sumHypotesis -> (1 / dataSize) * sumHypotesis)
                >> (\squaredError -> ( squaredError, param - learningRate * squaredError ))
            )


padFeatures : Matrix Float -> Matrix Float
padFeatures xs =
    xs
        |> prependColumn (List.repeat (Matrix.height xs) 1)
        |> unwrap


descend : Matrix Float -> Vector Float -> Vector Float -> Result String ( Float, Vector Float )
descend xs ys parameters =
    let
        xs_ =
            padFeatures xs

        updatedParamsAndErrors =
            List.indexedMap (paramDescend xs_ ys parameters) parameters
                |> flatResultList
    in
    updatedParamsAndErrors
        |> Result.map
            (List.foldl
                (\( error, param ) ( totalErrors, params ) ->
                    ( totalErrors + error, params ++ [ param ] )
                )
                ( 0, [] )
            )


gradientDescend : Matrix Float -> Vector Float -> Vector Float -> Result String (Vector Float)
gradientDescend xs ys parameters =
    case descend xs ys parameters of
        Ok ( error, nextParameters ) ->
            if abs error < threshold then
                Ok nextParameters
            else
                gradientDescend xs ys nextParameters

        Err err ->
            Err err


type alias Model =
    Result String (Vector Float)


initialParameters : Matrix a -> List number
initialParameters xs =
    List.repeat (Matrix.width xs + 1) 0


train : Matrix Float -> Vector Float -> Model
train xs ys =
    gradientDescend xs ys (initialParameters xs)


predict : Matrix Float -> Model -> Result String (Vector Float)
predict xs =
    Result.andThen (hypotesis <| padFeatures xs)
