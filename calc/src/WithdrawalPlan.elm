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


view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tr []
                [ td []
                    [ makeView savings "Savings plan" model ]
                , td
                    [ style "border-left" "thin solid #000000"
                    , style "border-right" "thin solid #000000"
                    , style "width" "5%"
                    ]
                    []
                , td []
                    [ makeView (\rate regPay nYears -> seedCapitalNeeded rate regPay (nYears * 12)) "Withdrawal plan" model ]
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
