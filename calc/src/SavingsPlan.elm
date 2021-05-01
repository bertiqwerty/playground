module SavingsPlan exposing (..)

import Browser
import Calc exposing (balanceFromPrices, linearPrices)
import RatePayYearsModel exposing (Msg)
import RatePayYearsModel exposing (Model)
import RatePayYearsModel exposing (init)
import RatePayYearsModel exposing (update)
import RatePayYearsModel exposing (makeView)


savings : Float -> Float -> Int -> Float
savings rate regPay nYears =
    let
        prices =
            linearPrices rate nYears 1

        payments =
            List.repeat (nYears * 12) regPay
    in
    balanceFromPrices payments prices


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = makeView savings "Savings plan"}
