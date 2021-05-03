module SavingsPlan exposing (..)

import Browser
import Calc exposing (balanceFromPrices, linearPrices)
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)


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
    Browser.sandbox
        { init = init
        , update = update 999
        , view = makeView savings "Savings plan"
        }
