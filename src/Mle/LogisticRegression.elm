module Mle.LogisticRegression exposing (..)

import Helpers exposing (unwrap)
import Math.Constants
import Math.Matrix as Matrix exposing (..)
import Mle.Internal.Regression as Internal
import NumElm


type alias Model =
    Internal.Model


type alias Settings =
    Internal.Settings


defaultSettings : Settings
defaultSettings =
    Internal.defaultSettings


init : Settings -> Model
init =
    Internal.init


train : Matrix -> Vector -> Model -> Model
train =
    Internal.train hypotesis


predict : Matrix -> Model -> Result String Vector
predict =
    Internal.predict hypotesis


hypotesis : Matrix -> Vector -> Vector
hypotesis xs parameters =
    multiplyVector xs parameters
        |> Result.map (NumElm.map (\z _ _ -> 1 / (1 + Math.Constants.e ^ -z)))
        |> unwrap
