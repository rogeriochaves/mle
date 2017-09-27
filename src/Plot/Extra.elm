module Plot.Extra exposing (..)

import Html
import Plot exposing (..)


plotSeries : List (Series (List a) msg) -> Html.Html msg
plotSeries =
    flip viewSeries []


scatter : List Float -> List Float -> Series b msg
scatter xs ys =
    dots (\_ -> List.map2 circle xs ys)


plot : List Float -> List Float -> Series b msg
plot xs ys =
    line (\_ -> List.map2 clear xs ys)
