module Math.Matrix exposing (..)

import Helpers exposing (flatResultList, maybeFlatMap, unwrap, unwrapMaybe)
import List.Extra
import NumElm exposing (..)


height : NdArray -> Int
height matrix =
    NumElm.size matrix
        |> List.head
        |> Maybe.withDefault 0


width : NdArray -> Int
width matrix =
    NumElm.size matrix
        |> List.tail
        |> Maybe.andThen List.head
        |> Maybe.withDefault 0


transpose : NdArray -> NdArray
transpose matrix =
    NumElm.transpose matrix
        |> toList
        |> Result.andThen (NumElm.matrix Float64)
        |> unwrap


getColumn : Int -> NdArray -> Maybe NdArray
getColumn n matrix =
    NumElm.slice [ 0, n ] [ height matrix, n + 1 ] matrix


getAt : Int -> Int -> NdArray -> Maybe number
getAt i j =
    NumElm.get [ i, j ]


getColumns : List Int -> NdArray -> Maybe NdArray
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


multiplyVector : NdArray -> NdArray -> Result String NdArray
multiplyVector =
    multiply


multiply : NdArray -> NdArray -> Result String NdArray
multiply =
    NumElm.dot


prependColumn : NdArray -> NdArray -> Result String NdArray
prependColumn =
    NumElm.concatAxis 1


unsafeGetColumn : Int -> NdArray -> NdArray
unsafeGetColumn n =
    getColumn n >> unwrapMaybe ("could not get column " ++ Basics.toString n)


unsafeGetColumns : List Int -> NdArray -> NdArray
unsafeGetColumns ns =
    getColumns ns >> unwrapMaybe ("could not get columns " ++ Basics.toString ns)


toList : NdArray -> Result String (List (List Float))
toList matrix =
    NumElm.dataToString matrix
        |> String.filter ((/=) ']')
        |> String.filter ((/=) '[')
        |> String.split ","
        |> List.map String.toFloat
        |> flatResultList
        |> Result.map (List.Extra.groupsOf <| width matrix)
