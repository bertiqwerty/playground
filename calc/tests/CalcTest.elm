module CalcTest exposing (..)

import Calc exposing (..)
import Expect
import SavingsPlan exposing (savings)
import Test exposing (..)
import WithdrawalPlan exposing (seedCapitalNeeded)


testLast : Test
testLast =
    describe "last"
        [ test "last with 2 elts"
            (\_ -> Expect.equal (Debug.log "last of [0, 4]" (last [ 0, 4 ])) 4)
        , test "last with 1 elts"
            (\_ -> Expect.equal (last [ 4 ]) 4)
        , test "last with 10 elts"
            (\_ -> Expect.equal (last (List.repeat 9 0 ++ [ 4 ])) 4)
        , test "last with 0 elts"
            (\_ -> Expect.equal (last []) -1)
        ]


testLinearPricesOfYear : Test
testLinearPricesOfYear =
    describe "linear prices of year"
        [ test "length, of year rate 1"
            (\_ ->
                linearPricesOfYear 1 1
                    |> List.length
                    |> Expect.equal 12
            )
        , test "of year rate 1"
            (\_ ->
                linearPricesOfYear 1 1
                    |> last
                    |> Expect.within (Expect.Absolute 1.0e-5) 1
            )
        , test "of year rate 1.5"
            (\_ ->
                linearPricesOfYear 1.5 1
                    |> last
                    |> Expect.within (Expect.Absolute 1.0e-5) 1.5
            )
        ]


testLinearPrices : Test
testLinearPrices =
    describe "linear prices"
        [ test "rate 1"
            (\_ ->
                linearPrices 1 10 1
                    |> last
                    |> Expect.within (Expect.Absolute 1.0e-5) 1
            )
        , test "length"
            (\_ ->
                linearPrices 1 10 1
                    |> List.length
                    |> Expect.equal 120
            )
        , test "rate 1.05"
            (\_ ->
                linearPrices 1.05 10 1
                    |> last
                    |> Expect.within (Expect.Absolute 1.0e-5) 1.6288946267774
            )
        , test "first price"
            (\_ ->
                linearPrices 1.05 10 1
                    |> List.head
                    |> Maybe.withDefault -1
                    |> Expect.within (Expect.Absolute 1.0e-7) 1.0041666667
            )
        ]


testBalanceFromPrice : Test
testBalanceFromPrice =
    describe "balance from price"
        [ test "zero payments"
            (\_ ->
                balanceFromPrices [ 0, 0, 0 ] [ 1, 1, 1 ] 0
                    |> Expect.within (Expect.Absolute 1.0e-5) 0
            )
        , test "all one"
            (\_ ->
                balanceFromPrices [ 1, 1, 1 ] [ 1, 1, 1 ] 0
                    |> Expect.within (Expect.Absolute 1.0e-5) 3
            )
        , test "linear price of year"
            (\_ ->
                balanceFromPrices (List.repeat 12 100) (linearPrices 1.07 1 1) 0
                    |> Expect.within (Expect.Absolute 1.0e-5) 1237.5595063359
            )
        ]


testSeed : Test
testSeed =
    describe "seed"
        [ test "rate 1"
            (\_ ->
                seedCapitalNeeded 1 10 12 0
                    |> Expect.within (Expect.Absolute 1.0e-7) 120
            )
        , test "single month"
            (\_ ->
                seedCapitalNeeded 1.12 10 1 0
                    |> Expect.within (Expect.Absolute 1.0e-7) (10 / 1.01)
            )
        , test "two months"
            (\_ ->
                seedCapitalNeeded 1.12 10 2 0
                    |> Expect.within (Expect.Absolute 1.0e-5) ((10 / 1.01) + (10 / 1.01) / 1.0099)
            )
        , test "50 years"
            (\_ ->
                seedCapitalNeeded 1.03 2500 600 0
                    |> Expect.greaterThan 600000
            )
        ]


testSavings : Test
testSavings =
    let
        rate =
            1.05

        regPay =
            100

        nYears =
            1

        sc =
            seedCapitalNeeded rate regPay (nYears * 12) 0
    in
    describe "savings"
        [ test "initial capital"
            (\_ ->
                savings rate 0 nYears 100
                    |> Expect.within (Expect.Absolute 1.0e-7) (100 * rate ^ nYears)
            )
        , test "withdrawal"
            (\_ ->
                savings rate -regPay nYears sc
                    |> Expect.within (Expect.Absolute 1.0e-5) 0
            )
        ]
