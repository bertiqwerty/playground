module WithdrawalPlan exposing (..)

import Browser
import Calc exposing (linearPrices, relativePrices)
import List.Extra
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)


maxYears : Int
maxYears =
    999


seedCapitalNeeded : Float -> Float -> Int -> Float -> Float
seedCapitalNeeded rate regPay nMonths _ =
    let
        startPrice = 1

        linearPricesFullYears =
            linearPrices rate (ceiling (toFloat nMonths / 12)) startPrice

        prices =
            List.take nMonths linearPricesFullYears

        relPrices =
            relativePrices prices startPrice

        priceProducts =
            List.take (List.length relPrices - 1) relPrices
                |> List.Extra.scanl1 (*)
                |> List.map (\v -> 1 / v)
    in
    regPay * (List.sum priceProducts + 1 / List.product relPrices)


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
