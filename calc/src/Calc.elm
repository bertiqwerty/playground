module Calc exposing (balanceFromPrices, last, linearPrices, linearPricesOfYear, main)

import Array
import Browser
import Html exposing (Html, br, div, input, label, span, text)
import Html.Attributes exposing (for, id, style, value)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { rate : Float
    , regularPayment : Float
    , nYears : Int
    }


init : Model
init =
    { rate = 5
    , regularPayment = 100
    , nYears = 20
    }


last : List Float -> Float
last inputs =
    Array.fromList inputs
        |> Array.get (List.length inputs - 1)
        |> Maybe.withDefault -1


linearPrices : Float -> Int -> Float -> List Float
linearPrices rate nYears startPrice =
    let
        recurse : Int -> List Float -> List Float
        recurse y prices =
            let
                newPrices =
                    linearPricesOfYear rate (last prices)
            in
            if y < nYears then
                newPrices ++ recurse (y + 1) newPrices

            else
                []
    in
    recurse 0 [ startPrice ]


linearPricesOfYear : Float -> Float -> List Float
linearPricesOfYear rate startPrice =
    let
        nMonths =
            12

        priceIncrease =
            (startPrice * rate - startPrice) / toFloat nMonths
    in
    List.range 0 (nMonths - 1)
        |> List.map (\i -> toFloat i)
        |> List.map (\i -> startPrice + (i + 1) * priceIncrease)


balanceFromPrices : List Float -> List Float -> Float
balanceFromPrices payments prices =
    let
        payces =
            List.map2 Tuple.pair payments prices
    in
    last prices
        * (List.map (\( pa, pr ) -> pa / pr) payces
            |> List.sum
          )


finalBalance : Float -> Float -> Int -> Float
finalBalance rate regPay nYears =
    let
        prices =
            linearPrices rate nYears 1

        payments =
            List.repeat (nYears * 12) regPay
    in
    balanceFromPrices payments prices


type Msg
    = ChangedRate String
    | ChangedRegPay String
    | ChangedYears String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedRate newContent ->
            { model
                | rate =
                    String.toFloat newContent
                        |> Maybe.withDefault 0
            }

        ChangedRegPay newContent ->
            { model
                | regularPayment =
                    String.toFloat newContent
                        |> Maybe.withDefault 0
            }

        ChangedYears newContent ->
            { model
                | nYears =
                    String.toInt newContent
                        |> Maybe.withDefault 0
            }


view : Model -> Html Msg
view model =
    div []
        [ label [ for "rate" ] [ text "Interest rate in %" ]
        , br [] []
        , input
            [ id "rate"
            , value (String.fromFloat model.rate)
            , onInput ChangedRate
            ]
            []
        , br [] []
        , label [ for "regpay" ] [ text "Payment each month" ]
        , br [] []
        , input
            [ id "regpay"
            , value (String.fromFloat model.regularPayment)
            , onInput ChangedRegPay
            ]
            []
        , br [] []
        , label [ for "years" ] [ text "Years" ]
        , br [] []
        , input
            [ id "years"
            , value (String.fromInt model.nYears)
            , onInput ChangedYears
            ]
            []
        , br [] []
        , div [ style "font-weight" "bold" ]
            [ text
                (String.fromFloat
                    ((finalBalance (1 + (model.rate / 100))
                        model.regularPayment
                        model.nYears
                        * 100
                        |> round
                        |> toFloat
                     )
                        / 100
                    )
                )
            ]
        ]
