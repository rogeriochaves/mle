module LinearRegression exposing (..)

import Helpers exposing (flatResultList)
import Math.Matrix as Matrix exposing (..)
import Unwrap exposing (..)


learningRate : Float
learningRate =
    0.1


threshold : Float
threshold =
    0.001


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
                >> List.map2 (*) (Matrix.getColumn index xs |> unwrap "could not get column")
                >> List.foldl (+) 0
                >> (\sumHypotesis -> (1 / dataSize) * sumHypotesis)
                >> (\squaredError -> ( squaredError, param - learningRate * squaredError ))
            )


padFeatures : Matrix Float -> Matrix Float
padFeatures xs =
    xs
        |> prependColumn (List.repeat (Matrix.height xs) 1)
        |> Result.toMaybe
        |> unwrap "could not concat 1s to features list"


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
    let
        nextDescend ( error, nextParameters ) =
            if abs error < threshold then
                Ok nextParameters
            else
                gradientDescend xs ys nextParameters
    in
    descend xs ys parameters
        |> Result.andThen nextDescend


initialParameters : Matrix a -> Vector number
initialParameters xs =
    List.repeat (Matrix.width xs + 1) 0


linearRegression xs ys =
    { train =
        \() ->
            case gradientDescend xs ys (initialParameters xs) of
                Ok trainedParams ->
                    Ok
                        { params = trainedParams
                        , predict = \xs -> hypotesis (padFeatures xs) trainedParams
                        }

                Err err ->
                    Err err
    , params = initialParameters xs
    }
