module Math.Matrix exposing (..)

import Array
import Matrix exposing (..)


transpose : Matrix number -> Matrix number
transpose matrix =
    indexedFoldl
        (\i j current inverse ->
            Matrix.set j i current inverse
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
