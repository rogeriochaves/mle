module Mle.Internal.Regression exposing (..)

import Helpers exposing (..)
import Math.Matrix as Matrix exposing (..)
import Math.Vector as Vector exposing (..)


type alias HypotesisFunction =
    Matrix Float -> Vector Float -> Vector Float


threshold : Float
threshold =
    0.01


padFeatures : Matrix Float -> Matrix Float
padFeatures xs =
    xs
        |> prependColumn (List.repeat (Matrix.height xs) 1)
        |> unwrap


descend : HypotesisFunction -> Settings -> Matrix Float -> Vector Float -> Vector Float -> ( Float, Vector Float )
descend hypotesisFunction settings xs ys parameters =
    let
        xs_ =
            padFeatures xs

        dataSize =
            toFloat (Matrix.height xs_)

        cost =
            Vector.subtract (hypotesisFunction xs_ parameters) ys

        residual =
            List.foldl (+) 0 cost

        updatedParameters =
            Matrix.multiplyVector (Matrix.transpose xs_) cost
                |> Vector.scalarMultiply (settings.learningRate / dataSize)
                |> Vector.subtract parameters
    in
    ( residual, updatedParameters )


gradientDescend : HypotesisFunction -> Settings -> Matrix Float -> Vector Float -> Vector Float -> Int -> Result String (Vector Float)
gradientDescend hypotesisFunction settings xs ys parameters iteration =
    case descend hypotesisFunction settings xs ys parameters of
        ( residual, nextParameters ) ->
            if iteration > settings.maxIterations then
                Err ("Failed to converge at a maximum of " ++ toString settings.maxIterations ++ " iterations, try scaling the params and adjusting the learn rate")
            else if abs residual < threshold then
                Ok nextParameters
            else
                gradientDescend hypotesisFunction settings xs ys nextParameters (iteration + 1)


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


train : HypotesisFunction -> Matrix Float -> Vector Float -> Model -> Model
train hypotesisFunction xs ys =
    Result.andThen
        (\model ->
            gradientDescend hypotesisFunction model.settings xs ys (initialParameters xs) 0
                |> Result.map (\params -> { model | params = params })
        )


predict : HypotesisFunction -> Matrix Float -> Model -> Result String (Vector Float)
predict hypotesisFunction xs =
    Result.map (.params >> hypotesisFunction (padFeatures xs))
