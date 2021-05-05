module Plot exposing (main)

import Calc exposing (linearPrices)
import Html
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line


main : Html.Html msg
main =
    chart


chart : Html.Html msg
chart =
    LineChart.viewCustom
        { y = Axis.default 300 "Price" .price
        , x = Axis.default 500 "Month" .month
        , container = Container.default "line-chart-1"
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.none
        , events = Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ LineChart.line Colors.strongBlue Dots.none "prices" indexedPrices
        ]


type alias Data =
    { month : Float
    , price : Float
    }


indexedPrices : List Data
indexedPrices =
    List.indexedMap (\i y -> Data (Basics.toFloat i) y) (linearPrices 1.1 30 1)
