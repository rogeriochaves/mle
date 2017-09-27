module Task.Extra exposing (..)

import Html exposing (..)
import Platform exposing (Task)
import Task


type Msg
    = NoOp
    | Render (Result String (Html Msg))


taskProgram : Task String (Html Msg) -> Program Never (Maybe (Html Msg)) Msg
taskProgram viewTask =
    Html.program
        { init = ( Nothing, Task.attempt Render viewTask )
        , update =
            \msg model ->
                case msg of
                    NoOp ->
                        ( model, Cmd.none )

                    Render (Ok view) ->
                        ( Just view, Cmd.none )

                    Render (Err err) ->
                        ( Just (text <| toString err), Cmd.none )
        , subscriptions = always Sub.none
        , view = Maybe.withDefault (text "Loading...")
        }
