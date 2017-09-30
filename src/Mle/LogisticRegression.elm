module Mle.LogisticRegression exposing (..)

import Helpers exposing (..)
import Math.Constants
import Math.Matrix as Matrix exposing (..)
import Math.Vector as Vector exposing (..)


threshold : Float
threshold =
    0.01


hypotesis : Matrix Float -> Vector Float -> Vector Float
hypotesis xs parameters =
    multiplyVector xs parameters
        |> List.map (\z -> 1 / (1 + Math.Constants.e ^ -z))


padFeatures : Matrix Float -> Matrix Float
padFeatures xs =
    xs
        |> prependColumn (List.repeat (Matrix.height xs) 1)
        |> unwrap


descend : Settings -> Matrix Float -> Vector Float -> Vector Float -> ( Float, Vector Float )
descend settings xs ys parameters =
    let
        xs_ =
            padFeatures xs

        dataSize =
            toFloat (Matrix.height xs_)

        cost =
            Vector.subtract (hypotesis xs_ parameters) ys

        totalCost =
            List.foldl (+) 0 cost

        updatedParameters =
            Matrix.multiplyVector (Matrix.transpose xs_) cost
                |> Vector.scalarMultiply (settings.learningRate / dataSize)
                |> Vector.subtract parameters
    in
    ( totalCost, updatedParameters )


gradientDescend : Settings -> Matrix Float -> Vector Float -> Vector Float -> Int -> Result String (Vector Float)
gradientDescend settings xs ys parameters iteration =
    case descend settings xs ys parameters of
        ( error, nextParameters ) ->
            if iteration > settings.maxIterations then
                Err ("Failed to converge at a maximum of " ++ toString settings.maxIterations ++ " iterations, try scaling the params and adjusting the learn rate")
            else if abs error < threshold then
                Ok nextParameters
            else
                gradientDescend settings xs ys nextParameters (iteration + 1)


type alias Model =
    Result String { settings : Settings, params : Vector Float }


type alias Settings =
    { learningRate : Float, maxIterations : Int }


defaultSettings : Settings
defaultSettings =
    { learningRate = 0.1, maxIterations = 10000 }


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
    Result.map (.params >> hypotesis (padFeatures xs))
