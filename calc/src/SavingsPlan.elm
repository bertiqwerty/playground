module SavingsPlan exposing (..)

import Browser
import Calc exposing (linearPrices, relativePrices)
import List.Extra
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)


savings : Float -> Float -> Int -> Float -> Float
savings rate regPay nYears initialCapital =
    let
        startPrice =
            1

        prices =
            linearPrices rate nYears startPrice

        relPrices =
            relativePrices prices startPrice

        tailRelPrices =
            List.tail relPrices |> Maybe.withDefault []

        partialProds =
            List.reverse tailRelPrices |> List.Extra.scanl1 (*)
        
    in
    initialCapital * List.product relPrices + regPay * (List.sum partialProds + 1)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update 999
        , view = makeView savings "Savings plan" "Final capital" True
        }
