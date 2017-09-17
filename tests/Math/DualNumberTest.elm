module Math.DualNumberTest exposing (..)

import ElmTestBDDStyle exposing (..)
import Expect exposing (..)
import Math.DualNumber exposing (..)
import Test exposing (..)


-- You can validate all derivatives using Wolfram Alpha, example:
-- http://www.wolframalpha.com/input/?i=d%2Fdx+f(x)%3D3%2Bx-(x%5E2%2F2x),x%3D72


suite : Test
suite =
    describe "Derivation using DualNumbers"
        [ it "derives functions of power of 2" <|
            expect (derivative (\x -> x ^ two) 3) to equal 6
        , it "derives functions of power of 3" <|
            expect (derivative (\x -> x ^ three) 3) to equal 27
        , it "derives functions of power of 2 with multiplication" <|
            expect (derivative (\x -> x * x) 3) to equal 6
        , it "derives multiplication functions" <|
            expect (derivative (\x -> two * x) 3) to equal 2
        , it "derives sum functions" <|
            expect (derivative (\x -> x + three) 2) to equal 1
        , it "derives division functions" <|
            expect (derivative (\x -> x / three) 5) to equal 0.3333333333333333
        , it "derives subtraction functions" <|
            expect (derivative (\x -> x - one) 12) to equal 1
        , it "derives complex functions" <|
            expect (derivative (\x -> three + x - (x ^ two / two * x)) 72) to equal -7775
        ]



--


(+) : DualNumber -> DualNumber -> DualNumber
(+) =
    sum


(-) : DualNumber -> DualNumber -> DualNumber
(-) =
    subtract


(^) : DualNumber -> DualNumber -> DualNumber
(^) =
    pow


(*) : DualNumber -> DualNumber -> DualNumber
(*) =
    multiply


(/) : DualNumber -> DualNumber -> DualNumber
(/) =
    divide
