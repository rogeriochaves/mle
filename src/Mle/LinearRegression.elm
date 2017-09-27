module Mle.LinearRegression exposing (..)

import Helpers exposing (..)
import Math.Matrix as Matrix exposing (..)


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


paramDescend : Settings -> Matrix Float -> Vector Float -> Vector Float -> Int -> Float -> Result String ( Float, Float )
paramDescend settings xs ys parameters index param =
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
                >> (\squaredError -> ( squaredError, param - settings.learningRate * squaredError ))
            )


padFeatures : Matrix Float -> Matrix Float
padFeatures xs =
    xs
        |> prependColumn (List.repeat (Matrix.height xs) 1)
        |> unwrap


descend : Settings -> Matrix Float -> Vector Float -> Vector Float -> Result String ( Float, Vector Float )
descend settings xs ys parameters =
    let
        xs_ =
            padFeatures xs

        updatedParamsAndErrors =
            List.indexedMap (paramDescend settings xs_ ys parameters) parameters
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


gradientDescend : Settings -> Matrix Float -> Vector Float -> Vector Float -> Int -> Result String (Vector Float)
gradientDescend settings xs ys parameters iteration =
    case descend settings xs ys parameters of
        Ok ( error, nextParameters ) ->
            if iteration > settings.maxIterations then
                Err ("Failed to converge at a maximum of " ++ toString settings.maxIterations ++ " iterations, try scaling the params and adjusting the learn rate")
            else if abs error < threshold then
                Ok nextParameters
            else
                gradientDescend settings xs ys nextParameters (iteration + 1)

        Err err ->
            Err err


type alias Model =
    Result String { settings : Settings, params : Vector Float }


type alias Settings =
    { learningRate : Float, maxIterations : Int }


defaultSettings : Settings
defaultSettings =
    { learningRate = 0.1, maxIterations = 1000 }


initialParameters : Matrix a -> List number
initialParameters xs =
    List.repeat (Matrix.width xs + 1) 0


init : Settings -> Model
init settings =
    Ok { params = [], settings = settings }


train : Matrix Float -> Vector Float -> Model -> Model
train xs ys =
    Result.andThen
        (\model ->
            gradientDescend model.settings xs ys (initialParameters xs) 0
                |> Result.map (\params -> { model | params = params })
        )


predict : Matrix Float -> Model -> Result String (Vector Float)
predict xs =
    Result.andThen (.params >> hypotesis (padFeatures xs))
