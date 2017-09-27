module Helpers exposing (..)

import Task


flatResultList : List (Result error a) -> Result error (List a)
flatResultList =
    List.foldl
        (\result total ->
            case ( result, total ) of
                ( Ok elem, Ok list ) ->
                    Ok (list ++ [ elem ])

                ( _, Err e ) ->
                    Err e

                ( Err e, _ ) ->
                    Err e
        )
        (Ok [])


maybeFlatMap : (a -> Maybe b) -> List a -> Maybe (List b)
maybeFlatMap f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
    List.foldr step (Just [])


unwrapMaybe : String -> Maybe a -> a
unwrapMaybe msg maybe =
    case maybe of
        Just x ->
            x

        _ ->
            Debug.crash ("unwrap failed " ++ toString maybe ++ ": " ++ msg)


unwrap : Result String value -> value
unwrap result =
    case result of
        Ok value ->
            value

        Err err ->
            Debug.crash err


resultToTask : Result x a -> Task.Task x a
resultToTask result =
    case result of
        Ok value ->
            Task.succeed value

        Err msg ->
            Task.fail msg
