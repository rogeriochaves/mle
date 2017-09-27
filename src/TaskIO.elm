module TaskIO exposing (..)

import Html exposing (..)
import Http
import Platform exposing (Task)
import Random exposing (Generator)
import Task
import Time


type alias TaskIO a =
    Task String a


type Msg
    = NoOp
    | Render (Result String (Html Msg))


program : TaskIO (Html Msg) -> Program Never (Maybe (Html Msg)) Msg
program viewTask =
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


return : a -> TaskIO a
return =
    Task.succeed >> Task.mapError toString


returnHttp : Http.Request a -> TaskIO a
returnHttp =
    Http.toTask >> Task.mapError toString


returnResult : Result a b -> TaskIO b
returnResult result =
    case result of
        Ok value ->
            Task.succeed value

        Err err ->
            Task.fail (toString err)


returnRandom : Generator a -> TaskIO a
returnRandom generator =
    Task.map
        (Tuple.first
            << Random.step generator
            << Random.initialSeed
            << round
        )
        Time.now
        |> Task.mapError toString


andThen : (a -> Task.Task x b) -> Task.Task x a -> Task.Task x b
andThen =
    Task.andThen
