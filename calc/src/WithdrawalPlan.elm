module WithdrawalPlan exposing (..)

import Browser
import Calc exposing (linearPrices)
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)


seedCapitalNeeded : Float -> Float -> Int -> Float
seedCapitalNeeded rate regPay nMonths =
    let
        prices =
            List.take nMonths (linearPrices rate (ceiling (toFloat nMonths / 12)) 1)

        shiftedPrices =
            List.concat [ [ 1 ], List.take (List.length prices - 1) prices ]

        relativePrices =
            List.map2 (\x y -> x / y) prices shiftedPrices

        priceProducts =
            List.map (\k -> 1 / (List.take k relativePrices |> List.product)) (List.range 1 (List.length relativePrices - 1))
    in
    regPay * (List.sum priceProducts + 1 / List.product relativePrices)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = makeView (\rate regPay nYears -> seedCapitalNeeded rate regPay (nYears * 12)) "Withdrawal plan"
        }
