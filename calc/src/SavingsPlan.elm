module SavingsPlan exposing (..)

import Browser
import Calc exposing (balanceFromPrices, linearPrices)
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)


savings : Float -> Float -> Int -> Float -> Float
savings rate regPay nYears initialCapital =
    let
        prices =
            linearPrices rate nYears 1

        payments =
            List.repeat (nYears * 12) regPay
    in
    balanceFromPrices payments prices initialCapital


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update 999
        , view = makeView savings "Savings plan" "Final capital" True
        }
