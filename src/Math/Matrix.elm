module Math.Matrix exposing (..)

import Helpers exposing (maybeFlatMap, unwrapMaybe)
import List.Extra
import Math.Vector exposing (..)


type alias Matrix a =
    List (List a)


map : (a -> b) -> Matrix a -> Matrix b
map =
    List.map << List.map


height : Matrix a -> Int
height matrix =
    List.length matrix


width : Matrix a -> Int
width matrix =
    case matrix of
        [] ->
            0

        line :: _ ->
            List.length line


size : Matrix a -> ( Int, Int )
size matrix =
    ( height matrix, width matrix )


transpose : Matrix a -> Matrix a
transpose matrix =
    -- TODO: use official List.Extra.transpose after <https://github.com/elm-community/list-extra/pull/83> is merged
    List.foldr (List.map2 (::)) (List.repeat (width matrix) []) matrix


getRow : Int -> Matrix a -> Maybe (Vector a)
getRow =
    List.Extra.getAt


getColumn : Int -> Matrix a -> Maybe (Vector a)
getColumn n =
    List.Extra.getAt n << transpose


getAt : Int -> Int -> Matrix a -> Maybe a
getAt i j matrix =
    getRow i matrix
        |> Maybe.andThen (List.Extra.getAt j)


getColumns : List Int -> Matrix a -> Maybe (Matrix a)
getColumns ns matrix =
    let
        transMatrix =
            transpose matrix
    in
    maybeFlatMap (\n -> List.Extra.getAt n transMatrix) ns
        |> Maybe.map transpose


multiplyVector : Matrix number -> Vector number -> Vector number
multiplyVector matrix vector =
    let
        multiplyRow row result =
            result ++ [ List.sum (List.map2 (*) vector row) ]
    in
    List.foldl multiplyRow [] matrix


multiply : Matrix number -> Matrix number -> Result String (Matrix number)
multiply a b =
    let
        bTrans =
            transpose b

        multiplyRow row result =
            result ++ [ multiplyVector bTrans row ]
    in
    if width a /= height b then
        Err <|
            "Matrix sizes are not fit for multiplication: Could not multiply a "
                ++ toString (size a)
                ++ " matrix with a "
                ++ toString (size b)
                ++ " matrix"
    else
        Ok (List.foldl multiplyRow [] a)


prependColumn : Vector a -> Matrix a -> Result String (Matrix a)
prependColumn column matrix =
    if List.length column /= height matrix then
        Err <|
            "Could not prepend a "
                ++ toString (List.length column)
                ++ "-sized vector to a "
                ++ toString (size matrix)
                ++ " matrix"
    else
        Ok <| transpose (column :: transpose matrix)


unsafeGetColumn : Int -> Matrix a -> Vector a
unsafeGetColumn n =
    getColumn n >> unwrapMaybe ("could not get column " ++ toString n)


unsafeGetColumns : List Int -> Matrix a -> Matrix a
unsafeGetColumns ns =
    getColumns ns >> unwrapMaybe ("could not get columns " ++ toString ns)
