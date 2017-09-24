module Math.Matrix exposing (..)

import Helpers exposing (maybeFlatMap)
import List.Extra


type alias Matrix a =
    List (List a)


type alias Vector a =
    List a


height : Matrix a -> Int
height matrix =
    List.length matrix


width : Matrix a -> Int
width matrix =
    List.head matrix
        |> Maybe.withDefault []
        |> List.length


size : Matrix a -> ( Int, Int )
size matrix =
    ( height matrix, width matrix )


transpose : Matrix a -> Matrix a
transpose =
    List.Extra.transpose


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
