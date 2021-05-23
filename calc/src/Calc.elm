module Calc exposing (balanceFromPrices, last, linearPrices, linearPricesOfYear)

import Array


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
            (rate - 1) / toFloat nMonths
    in
    List.range 0 (nMonths - 1)
        |> List.map (\i -> toFloat i)
        |> List.map (\i -> startPrice * (1 + (i + 1) * priceIncrease))


balanceFromPrices : List Float -> List Float -> Float -> Float
balanceFromPrices payments prices initialCapital =
    let
        payces =
            List.map2 Tuple.pair payments prices
    in
    ((List.map (\( pa, pr ) -> pa / pr) payces
        |> List.sum
     )
        + initialCapital
    )
        * last prices
