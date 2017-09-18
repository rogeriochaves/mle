module Unwrap exposing (..)


unwrap : String -> Maybe a -> a
unwrap msg maybe =
    case maybe of
        Just x ->
            x

        _ ->
            Debug.crash ("unwrap failed " ++ toString maybe ++ ": " ++ msg)
