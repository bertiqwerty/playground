module WithdrawalPlan exposing (..)

import Browser
import Calc exposing (linearPrices)
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)


seedCapitalNeeded : Float -> Float -> Int -> Float -> Float
seedCapitalNeeded rate regPay nMonths _ =
    let
        startCapital =
            1

        prices =
            List.take nMonths (linearPrices rate (ceiling (toFloat nMonths / 12)) startCapital)

        shiftedPrices =
            List.concat [ [ startCapital ], List.take (List.length prices - 1) prices ]

        relativePrices =
            List.map2 (\x y -> x / y) prices shiftedPrices

        mapping k =
            1 / (List.take k relativePrices |> List.product)

        priceProducts =
            List.map mapping (List.range 1 (List.length relativePrices - 1))
    in
    regPay * (List.sum priceProducts + 1 / List.product relativePrices)


seedCapitalYears : Float -> Float -> Int -> Float -> Float
seedCapitalYears rate regPay nYears initialCapital =
    seedCapitalNeeded rate regPay (nYears * 12) initialCapital


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update 399
        , view = makeView seedCapitalYears "Withdrawal plan" "Seed capital needed " False
        }
