module Main exposing (..)

import Browser
import Models exposing (Build, Msg(..), JobData, parseData)
import Duration
import Html.Attributes exposing (class)
import Html exposing (..)
import ApiClient exposing (requestData)
import LineChart exposing (Config)
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import Color
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import Svg
import Svg.Attributes as Attributes
import Time exposing (toHour, toMinute, toSecond, utc)
import Loading exposing (LoaderType(..), defaultConfig, render)

---- PROGRAM ----

main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


---- MODEL ----

type alias Model = { response : Response, data: JobData, hovering: List Build }

type Response = Initial
    | Loading
    | Failure
    | Success


init : ( Model, Cmd Msg )
init =
    ( initModel, requestData )

initModel : Model
initModel = { response = Loading, data = JobData [] 0 [], hovering = [] }

---- UPDATE ----

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Data data ->
            case data of
                Ok d ->
                    ( { model | response = Success, data = (asJobData model d) }, Cmd.none )
                Err _ -> ( { model | response = Failure }, Cmd.none )
        Hover hovering -> ( { model | hovering = hovering }, Cmd.none )

asJobData : Model -> List Build -> JobData
asJobData model data =
    let branches = model.data.branches
        time = model.data.totalTime
        jobs = model.data.jobs
    in
    parseData (branches, time, jobs) data

---- VIEW ----

view : Model -> Html Msg
view model =
    div []
        [ render model.response model ]

render : Response -> Model -> Html Msg
render res model =
    case res of
        Initial -> h1 [] [ text "Initial" ]
        Loading -> renderLoadingIcon
        Failure -> h1 [] [ text "Error" ]
        Success -> div [] [
            h1 [class "metadata"] [ span [class "span"] [ text "Scheduled jobs: " ], text (formatJobsCount model.data.jobs) ] ,
            h1 [class "metadata"] [ span [class "span"] [ text "Total Runtime: " ], text (formatTime model.data.totalTime) ] ,
            h1 [class "metadata"] [ span [class "span"] [ text "Branches:" ] ] ,
            ul [class "metadata"] [ formatBranches model.data.branches ] ,
            renderChart model (orderByBuildNum model.data.jobs)
            ]

renderLoadingIcon : Html Msg
renderLoadingIcon =
    div [class "loading"] [
        Loading.render BouncingBalls { defaultConfig | color = "#ff003d", size = 40 } Loading.On
    ]

orderByBuildNum : List Build -> List Build
orderByBuildNum list =
    list |> List.sortBy (\b -> b.num)

renderChart : Model -> List Build -> Html.Html Msg
renderChart model jobs =
    LineChart.viewCustom
        { y = customAxis
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
          , junk = Junk.hoverMany model.hovering tooltipTitle tooltipTime
          , grid = Grid.default
          , area = Area.default
          , line = Line.default
          , dots = Dots.hoverMany model.hovering
          }
        [ LineChart.line colorUndoRed Dots.circle "Time" jobs ]

tooltipTitle : Build -> String
tooltipTitle build =
    "Branch: " ++ build.branch

tooltipTime : Build -> String
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

formatBranches : List String -> Html msg
formatBranches lst =
    lst
       |> List.map (\l -> li [] [ text l ])
       |> ul []

formatTime : Int -> String
formatTime time =
    time
        |> Time.millisToPosix
        |> toUtcString

toUtcString : Time.Posix -> String
toUtcString time =
    String.fromInt (toHour utc time)
    ++ "h " ++
    String.fromInt (toMinute utc time)
    ++ "min " ++
    String.fromInt (toSecond utc time)
    ++ "sec"

formatJobsCount : List Build -> String
formatJobsCount jobs =
    jobs
        |> List.length
        |> String.fromInt

customAxis : Axis.Config Build msg
customAxis =
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