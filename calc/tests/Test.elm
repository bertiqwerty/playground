module Example exposing (..)

import Calc exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


testLast : Test
testLast =
    describe "last"
        [ test "last with 2 elts"
            (\_ -> Expect.equal (last [ 0, 4 ]) 4)
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
                    |> Expect.within (Expect.Absolute 1e-5) 1
            )
        , test "of year rate 1.5"
            (\_ ->
                linearPricesOfYear 1.5 1
                    |> last
                    |> Expect.within (Expect.Absolute 1e-5) 1.5
            )
        ]


testLinearPrices : Test
testLinearPrices =
    describe "linear prices"
        [ test "rate 1"
            (\_ ->
                linearPrices 1 10 1
                    |> last
                    |> Expect.within (Expect.Absolute 1e-5) 1
            )
        , test "rate 1.05"
            (\_ ->
                linearPrices 1.05 10 1
                    |> last
                    |> Expect.within (Expect.Absolute 1e-5) 1.6288946267774
            )
        ]

testBalanceFromPrice : Test
testBalanceFromPrice =
    describe "balance from price"
        [ test "zero payments"
            (\_ ->
                balanceFromPrices [0, 0, 0] [1, 1, 1]
                    |> Expect.within (Expect.Absolute 1e-5) 0
            )
        , test "all one"
            (\_ ->
                balanceFromPrices [1, 1, 1] [1, 1, 1]
                    |> Expect.within (Expect.Absolute 1e-5) 3
            )
            , test "linear price of year"
            (\_ ->
                balanceFromPrices (List.repeat 12 100) (linearPrices 1.07 1 1)
                    |> Expect.within (Expect.Absolute 1e-5) 1237.5595063359
            )
        ]