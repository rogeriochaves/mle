module Math.Matrix exposing (..)

import Helpers exposing (flatResultList, maybeFlatMap, unwrap, unwrapMaybe)
import List.Extra
import NumElm exposing (..)


type alias Matrix =
    NdArray


type alias Vector =
    NdArray


vec : List number -> NumElm.NdArray
vec =
    NumElm.vector Float64 >> unwrap


mat : List (List number) -> NumElm.NdArray
mat =
    NumElm.matrix Float64 >> unwrap


height : Matrix -> Int
height matrix =
    NumElm.size matrix
        |> List.head
        |> Maybe.withDefault 0


width : Matrix -> Int
width matrix =
    NumElm.size matrix
        |> List.tail
        |> Maybe.andThen List.head
        |> Maybe.withDefault 0


transpose : Matrix -> Matrix
transpose matrix =
    NumElm.transpose matrix
        |> toList
        |> Result.andThen (NumElm.matrix Float64)
        |> unwrap


getColumn : Int -> Matrix -> Maybe Matrix
getColumn n matrix =
    NumElm.slice [ 0, n ] [ height matrix, n + 1 ] matrix


getColumns : List Int -> Matrix -> Maybe Matrix
getColumns ns matrix =
    let
        concatColumns columns =
            let
                tail =
                    List.tail columns |> Maybe.withDefault []
            in
            List.head columns
                |> Maybe.andThen (tailHeadConcat tail)

        tailHeadConcat tail head =
            List.foldl
                (flip (NumElm.concatAxis 1) >> Result.andThen)
                (Ok head)
                tail
                |> Result.toMaybe
    in
    maybeFlatMap (flip getColumn matrix) ns
        |> Maybe.andThen concatColumns


unsafeGetColumn : Int -> Matrix -> Matrix
unsafeGetColumn n =
    getColumn n >> unwrapMaybe ("could not get column " ++ Basics.toString n)


unsafeGetColumns : List Int -> Matrix -> Matrix
unsafeGetColumns ns =
    getColumns ns >> unwrapMaybe ("could not get columns " ++ Basics.toString ns)


toList : Matrix -> Result String (List (List Float))
toList matrix =
    NumElm.dataToString matrix
        |> String.filter ((/=) ']')
        |> String.filter ((/=) '[')
        |> String.split ","
        |> List.map String.toFloat
        |> flatResultList
        |> Result.map (List.Extra.groupsOf <| width matrix)
