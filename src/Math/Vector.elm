module Math.Vector exposing (..)


type alias Vector a =
    List a


subtract : Vector number -> Vector number -> Vector number
subtract a b =
    List.map2 (\a b -> a - b) a b


scalarMultiply : number -> Vector number -> Vector number
scalarMultiply value =
    List.map ((*) value)
