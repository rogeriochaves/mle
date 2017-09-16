module DualNumber exposing (..)

-- source: https://codon.com/automatic-differentiation-in-ruby


type DualNumber
    = DualNumber Float Float


derivative : (DualNumber -> DualNumber) -> Float -> Float
derivative fn x =
    case fn (DualNumber x 1) of
        DualNumber real dual ->
            dual


sum : DualNumber -> DualNumber -> DualNumber
sum (DualNumber real dual) (DualNumber real_ dual_) =
    DualNumber (real + real_) (dual + dual_)


subtract : DualNumber -> DualNumber -> DualNumber
subtract (DualNumber real dual) (DualNumber real_ dual_) =
    DualNumber (real - real_) (dual - dual_)


multiply : DualNumber -> DualNumber -> DualNumber
multiply (DualNumber real dual) (DualNumber real_ dual_) =
    DualNumber (real * real_) (real * dual_ + dual * real_)


divide : DualNumber -> DualNumber -> DualNumber
divide (DualNumber real dual) (DualNumber real_ dual_) =
    DualNumber (real / real_) ((dual * real_ - real * dual_) / (real_ * real_))


pow : DualNumber -> DualNumber -> DualNumber
pow (DualNumber real dual) (DualNumber power _) =
    DualNumber (real ^ power) (power * dual * (real ^ (power - 1)))


toDual : Float -> DualNumber
toDual number =
    DualNumber number 0


one : DualNumber
one =
    toDual 1


two : DualNumber
two =
    toDual 2


three : DualNumber
three =
    toDual 3


four : DualNumber
four =
    toDual 4


five : DualNumber
five =
    toDual 5
