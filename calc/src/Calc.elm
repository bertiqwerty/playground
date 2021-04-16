module Calc exposing (main, last, linearPricesOfYear, linearPrices, balanceFromPrices)

import Browser
import Html exposing (Html, br, div, input, label, text)
import Html.Attributes exposing (for, id, value)
import Html.Events exposing (onInput)
import Array

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
        rec : Int -> List Float -> List Float
        rec y prices =
            let
                new_prices =
                    if y < nYears then linearPricesOfYear rate (last prices) else []
            in
            if y < nYears then new_prices ++ (rec (y + 1) new_prices) else []
    in
    rec 0 [startPrice]


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
    
    (last prices) * (List.map (\( pa, pr ) -> pa / pr) payces
        |> List.sum)


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
            { model | rate = Maybe.withDefault 0 (String.toFloat newContent) }

        ChangedRegPay newContent ->
            { model | regularPayment = Maybe.withDefault 0 (String.toFloat newContent) }

        ChangedYears newContent ->
            { model | nYears = Maybe.withDefault 0 (String.toInt newContent) }


view : Model -> Html Msg
view model =
    div []
        [ label [ for "rate" ] [ text "Interest rate in %" ]
        , br [] []
        , input [ id "rate", value (String.fromFloat model.rate), onInput ChangedRate ] []
        , br [] []
        , label [ for "regpay" ] [ text "Regular payments" ]
        , br [] []
        , input [ id "regpay", value (String.fromFloat model.regularPayment), onInput ChangedRegPay ] []
        , br [] []
        , label [ for "years" ] [ text "Years" ]
        , br [] []
        , input [ id "years", value (String.fromInt model.nYears), onInput ChangedYears ] []
        , br [] []
        , div [] [ text (String.fromFloat (finalBalance (1 + (model.rate / 100)) model.regularPayment model.nYears)) ]
        ]
