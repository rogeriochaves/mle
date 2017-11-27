module TestHelpers exposing (..)

import Fuzz exposing (..)
import Helpers exposing (unwrap)
import Math.Matrix exposing (..)
import NumElm exposing (Dtype(Float64))


fuzzMatrix : Int -> Int -> Fuzzer Matrix
fuzzMatrix rows columns =
    List.repeat rows (fuzzList columns)
        |> sequenceFuzzers
        |> Fuzz.map mat


fuzzVector : Int -> Fuzzer Vector
fuzzVector length =
    fuzzList length
        |> Fuzz.map vec


fuzzList : Int -> Fuzzer (List Float)
fuzzList length =
    List.repeat length Fuzz.float
        |> sequenceFuzzers


sequenceFuzzers : List (Fuzzer a) -> Fuzzer (List a)
sequenceFuzzers =
    List.foldr (Fuzz.map2 (::)) (Fuzz.constant [])
