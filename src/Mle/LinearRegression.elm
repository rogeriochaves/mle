module Mle.LinearRegression exposing (..)

import Math.Matrix as Matrix exposing (..)
import Math.Vector as Vector exposing (..)
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


train : Matrix Float -> Vector Float -> Model -> Model
train =
    Internal.train hypotesis


predict : Matrix Float -> Model -> Result String (Vector Float)
predict =
    Internal.predict hypotesis


hypotesis : Matrix Float -> Vector Float -> Vector Float
hypotesis xs parameters =
    multiplyVector xs parameters
