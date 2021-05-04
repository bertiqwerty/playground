module RatePayYearsModel exposing (..)

import Html exposing (Html, br, div, input, label, text)
import Html.Attributes exposing (for, id, style, value)
import Html.Events exposing (onInput)


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


type Msg
    = ChangedRate String
    | ChangedRegPay String
    | ChangedYears String


update : Int -> Msg -> Model -> Model
update maxYears msg model =
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
                        |> (\x ->
                                if x < maxYears then
                                    x

                                else
                                    maxYears
                           )
            }


makeView : (Float -> Float -> Int -> Float) -> String -> String -> Model -> Html Msg
makeView computeBalance name finalLabel model =
    div []
        [ div [ style "font-weight" "bold" ] [ text name ]
        , br [] []
        , label [ for "rate" ] [ text "Interest rate in %" ]
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
        , div [] [ text finalLabel ]
        , div [style "font-weight" "bold"]
            [ text (String.fromFloat
                ((computeBalance (1 + (model.rate / 100))
                    model.regularPayment
                    model.nYears
                    * 100
                    |> round
                    |> toFloat
                 )
                    / 100
                ))
            ]
        ]
