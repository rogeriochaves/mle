module Task.Extra exposing (..)

import Html exposing (..)
import Platform exposing (Task)
import Task


type Msg
    = NoOp
    | Result (Html Msg)


taskProgram : Task.Task Never (Html Msg) -> Program Never (Maybe (Html Msg)) Msg
taskProgram viewTask =
    Html.program
        { init = ( Nothing, Task.perform Result viewTask )
        , update =
            \msg model ->
                case msg of
                    NoOp ->
                        ( model, Cmd.none )

                    Result view ->
                        ( Just view, Cmd.none )
        , subscriptions = always Sub.none
        , view = Maybe.withDefault (text "Loading...")
        }
