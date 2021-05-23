module WithdrawalPlan exposing (..)

import Browser
import Calc exposing (linearPrices)
import List.Extra
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)


maxYears : Int
maxYears =
    399


seedCapitalNeeded : Float -> Float -> Int -> Float -> Float
seedCapitalNeeded rate regPay nMonths _ =
    let
        startCapital =
            1

        checkedLinearPrices =
            linearPrices rate (ceiling (toFloat nMonths / 12)) startCapital

        prices =
            List.take nMonths checkedLinearPrices

        shiftedPrices =
            List.concat [ [ startCapital ], List.take (List.length prices - 1) prices ]

        relativePrices =
            List.map2 (\x y -> x / y) prices shiftedPrices

        priceProducts =
            List.take (List.length relativePrices - 1) relativePrices
                |> List.Extra.scanl1 (*)
                |> List.map (\v -> 1 / v)
    in
    regPay * (List.sum priceProducts + 1 / List.product relativePrices)


seedCapitalYears : Float -> Float -> Int -> Float -> Float
seedCapitalYears rate regPay nYears initialCapital =
    seedCapitalNeeded rate regPay (nYears * 12) initialCapital


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update maxYears
        , view = makeView seedCapitalYears "Withdrawal plan" "Seed capital needed " False
        }
