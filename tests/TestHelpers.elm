module TestHelpers exposing (..)

import Fuzz exposing (..)
import Math.Matrix exposing (..)
import Math.Vector exposing (..)


fuzzMatrix : Int -> Int -> Fuzzer (Matrix Float)
fuzzMatrix rows columns =
    List.repeat rows (fuzzVector columns)
        |> sequenceFuzzers


fuzzVector : Int -> Fuzzer (Vector Float)
fuzzVector length =
    List.repeat length Fuzz.float
        |> sequenceFuzzers


sequenceFuzzers : List (Fuzzer a) -> Fuzzer (List a)
sequenceFuzzers =
    List.foldr (Fuzz.map2 (::)) (Fuzz.constant [])
