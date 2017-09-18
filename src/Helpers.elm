module Helpers exposing (..)


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
