module Calc exposing (last, linearPrices, linearPricesOfYear, relativePrices)

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


relativePrices : List Float -> Float -> List Float
relativePrices prices startPrice =
    let
        shiftedPrices =
            List.concat [ [ startPrice ], List.take (List.length prices - 1) prices ]
    in
    List.map2 (\x y -> x / y) prices shiftedPrices


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

