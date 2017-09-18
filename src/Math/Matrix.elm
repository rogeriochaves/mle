module Math.Matrix exposing (..)

import Array exposing (Array)
import Array.Extra
import Matrix exposing (..)


transpose : Matrix number -> Matrix number
transpose matrix =
    indexedFoldl
        (\x y elem inverse ->
            Matrix.set y x elem inverse
        )
        (Matrix.repeat (Matrix.height matrix) (Matrix.width matrix) 0)
        matrix


foldl : (number -> b -> b) -> b -> Matrix number -> b
foldl fn initial matrix =
    Array.foldl fn initial matrix.data


indexedFoldl : (Int -> Int -> number -> b -> b) -> b -> Matrix number -> b
indexedFoldl fn initial matrix =
    let
        fn_ curr acc =
            let
                i =
                    acc.index

                x =
                    i % width matrix

                y =
                    i // width matrix
            in
            { index = i + 1, acc = fn x y curr acc.acc }
    in
    Array.foldl fn_ { index = 0, acc = initial } matrix.data
        |> .acc


multiply : Matrix number -> Matrix number -> Maybe (Matrix number)
multiply a b =
    let
        initial =
            Matrix.repeat (Matrix.width b) (Matrix.height a) 0

        mult x y _ result =
            case ( result, Matrix.getRow y a, Matrix.getColumn x b ) of
                ( Just matrix, Just row, Just column ) ->
                    let
                        value =
                            Array.Extra.map2 (\x y -> x * y) row column
                                |> Array.foldl (+) 0
                    in
                    Just (Matrix.set x y value matrix)

                _ ->
                    Nothing
    in
    indexedFoldl mult (Just initial) initial


addColumn : Array number -> Matrix number -> Maybe (Matrix number)
addColumn column matrix =
    let
        initial =
            Matrix.repeat (Matrix.width matrix + 1) (Matrix.height matrix) 0

        addCol x y _ result =
            case result of
                Just newMatrix ->
                    if x == 0 then
                        Array.get y column
                            |> Maybe.map (\newElem -> Matrix.set x y newElem newMatrix)
                    else
                        Matrix.get (x - 1) y matrix
                            |> Maybe.map (\elem -> Matrix.set x y elem newMatrix)

                Nothing ->
                    Nothing
    in
    indexedFoldl addCol (Just initial) initial
