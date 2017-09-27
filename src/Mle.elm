module Mle exposing (..)

import Html exposing (..)
import Random
import Random.Int


program : (Random.Seed -> Html (Maybe Int)) -> Program Never (Maybe Int) (Maybe Int)
program view =
    Html.program
        { init = ( Nothing, Random.Int.anyInt |> Random.generate Just )
        , update = \model _ -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        , view =
            \seed ->
                case seed of
                    Just seed ->
                        view (Random.initialSeed seed)

                    Nothing ->
                        text ""
        }
