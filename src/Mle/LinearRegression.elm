module Mle.LinearRegression exposing (..)

import Helpers exposing (unwrap)
import Math.Matrix as Matrix exposing (..)
import Mle.Internal.Regression as Internal


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
        |> unwrap
