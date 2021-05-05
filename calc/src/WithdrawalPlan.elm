module WithdrawalPlan exposing (..)

import Browser
import Calc exposing (linearPrices)
import Html exposing (Html, div, table, td, tr)
import Html.Attributes exposing (style)
import RatePayYearsModel exposing (Model, Msg, init, makeView, update)
import SavingsPlan exposing (savings)


seedCapitalNeeded : Float -> Float -> Int -> Float
seedCapitalNeeded rate regPay nMonths =
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


view : Model -> Html Msg
view model =
    let
        seedCapitalYears rate regPay nYears =
            seedCapitalNeeded rate regPay (nYears * 12)
    in
    div [ style "background-color" "200 100 100" ]
        [ table []
            [ tr []
                [ td []
                    [ makeView savings "Savings plan" "Resulting capital " model ]
                , td
                    [ style "border-left" "thin solid #000000"
                    , style "border-right" "thin solid #000000"
                    , style "width" "5%"
                    ]
                    []
                , td []
                    [ makeView seedCapitalYears "Withdrawal plan" "Seed capital needed " model ]
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update 99
        , view = view
        }
