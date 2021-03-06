module Charts exposing (renderChart)


import Color
import Duration
import Html
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Models exposing (Job, Msg(..))
import Svg
import Svg.Attributes as Attributes
import Time exposing (toMinute, toSecond, utc)


renderChart : List Job -> List Job -> Html.Html Msg
renderChart hovering jobs =
    LineChart.viewCustom
        { y = customYAxis
          , x = Axis.default 1850 "id" (toFloat << .num)
          , container = Container.responsive "circle-ci-jobs"
          , interpolation = Interpolation.linear
          , intersection = Intersection.default
          , legends = Legends.grouped .max .min -20 -620
          , events =
                Events.custom
                    [ Events.onMouseMove Hover Events.getNearestX
                    , Events.onMouseLeave (Hover [])
                    ]
          , junk = Junk.hoverMany hovering tooltipTitle tooltipTime
          , grid = Grid.default
          , area = Area.default
          , line = Line.default
          , dots = Dots.hoverMany hovering
          }
        [ LineChart.line colorUndoRed Dots.circle "Time" jobs ]

tooltipTitle : Job -> String
tooltipTitle build =
    "Branch: " ++ build.branch

tooltipTime : Job -> String
tooltipTime build =
    build.time
        |> Time.millisToPosix
        |> formatMinutes

formatMinutes : Time.Posix -> String
formatMinutes posix =
    String.fromInt (toMinute utc posix)
    ++ "min " ++
    String.fromInt (toSecond utc posix)
    ++ "sec"

colorUndoRed : Color.Color
colorUndoRed = Color.rgb255 255 0 61

customYAxis : Axis.Config Job msg
customYAxis =
  Axis.custom
    { title = Title.default "minutes"
    , variable = Just << (Duration.inMinutes << Duration.milliseconds << toFloat << .time)
    , pixels = 750
    , range = Range.padded 20 20
    , axisLine = AxisLine.rangeFrame Color.gray
    , ticks =
            Ticks.custom <| \range _ ->
              let minuteNumbers = List.range (ceiling range.min) (floor range.max)
                  secondNumbers =
                    List.range (ceiling range.min) (floor range.max - 1)
                        |> List.concatMap toQuarters

                  toQuarters min = [ toQuarter min 0.25, toQuarter min 0.5, toQuarter min 0.75 ]
                  toQuarter min offset = toFloat min + offset
              in
              List.map Tick.int minuteNumbers ++
              List.map secondTick secondNumbers
    }

secondTick : Float -> Tick.Config msg
secondTick position =
  Tick.custom
    { position = position
    , color = Color.gray
    , width = 1
    , length = 5
    , grid = True
    , direction = Tick.negative
    , label =
        let minute = toFloat (floor position)
            offset = position - minute
            seconds = 60 * offset
        in
        Just (Svg.text_
                  [ Attributes.fill (Color.toCssString colorUndoRed)
                  , Attributes.style "pointer-events: none;"
                  , Attributes.fontSize "10"
                  ]
                  [ Svg.tspan [] [ Svg.text (String.fromFloat seconds) ] ])
    }