module RatePayYearsModel exposing (..)

import Html exposing (Html, br, div, input, label, table, td, text, tr)
import Html.Attributes exposing (class, colspan, for, id, style, value)
import Html.Events exposing (onInput)


type alias Model =
    { initialCapital : Float
    , rate : Float
    , regularPayment : Float
    , nYears : Int
    }


init : Model
init =
    { initialCapital = 0
    , rate = 5
    , regularPayment = 100
    , nYears = 20
    }


type Msg
    = ChangedRate String
    | ChangedRegPay String
    | ChangedYears String
    | ChangedInitialCapital String


update : Int -> Msg -> Model -> Model
update maxYears msg model =
    let
        makePositive x = if x < 0  then -x else x
    in
    
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
                        |> makePositive
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
                        |> makePositive
            }

        ChangedInitialCapital newContent ->
            { model
                | initialCapital =
                    String.toFloat newContent
                        |> Maybe.withDefault 0
                        |> makePositive
            }


makeView : (Float -> Float -> Int -> Float -> Float) -> String -> String -> Bool -> Model -> Html Msg
makeView computeBalance name finalLabel withInitialCapital model =
    let
        firstChildren =
            if withInitialCapital then
                [ tr []
                    [ td [ class "wlabel" ]
                        [ label [ for "initialCapital" ] [ text "Initial capital" ]
                        ]
                    ]
                , tr []
                    [ td [ class "winput" ]
                        [ input
                            [ id "initialCapital"
                            , value (String.fromFloat model.initialCapital)
                            , onInput ChangedInitialCapital
                            ]
                            []
                        ]
                    ]
                ]

            else
                [ tr []
                    [ td [ class "wlabeldummy" ]
                        [ label [] [ text "-" ] ]
                    ]
                , tr []
                    [ td [ class "winputdummy" ]
                        [ input
                            [ value "-"
                            ]
                            []
                        ]
                    ]
                ]
    in
    div []
        [ table []
            (tr [] [ td [ class "top" ] [ text name ] ]
                :: firstChildren
                ++ [ tr []
                        [ td [ class "wlabel" ]
                            [ label [ for "rate" ] [ text "Interest rate in %" ]
                            ]
                        ]
                   , tr []
                        [ td [ class "winput" ]
                            [ input
                                [ id "rate"
                                , value (String.fromFloat model.rate)
                                , onInput ChangedRate
                                ]
                                []
                            ]
                        ]
                   , tr []
                        [ td [ class "wlabel" ]
                            [ label [ for "regpay" ] [ text "Payment each month" ]
                            ]
                        ]
                   , tr []
                        [ td [ class "winput" ]
                            [ input
                                [ id "regpay"
                                , value (String.fromFloat model.regularPayment)
                                , onInput ChangedRegPay
                                ]
                                []
                            ]
                        ]
                   , tr []
                        [ td [ class "wlabel" ]
                            [ label [ for "years" ] [ text "Years" ]
                            ]
                        ]
                   , tr []
                        [ td [ class "winput" ]
                            [ input
                                [ id "years"
                                , value (String.fromInt model.nYears)
                                , onInput ChangedYears
                                ]
                                []
                            ]
                        ]
                   , tr []
                        [ td [ ]
                            [ text finalLabel ]
                        ]
                   , tr []
                        [ td [ class "result" ]
                            [ text
                                (String.fromFloat
                                        ((computeBalance (1 + (model.rate / 100))
                                            model.regularPayment
                                            model.nYears
                                            model.initialCapital
                                            * 100
                                            |> round
                                            |> toFloat
                                         )
                                            / 100
                                        )
                                )
                            ]
                        ]
                   ]
            )
        ]
