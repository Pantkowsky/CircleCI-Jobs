module Main exposing (..)

import Browser
import Duration
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (..)
import Api.ApiClient
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
import Set
import Time exposing (toHour, toMinute, toSecond, utc)
import Loading exposing (LoaderType(..), defaultConfig, render)

---- PROGRAM ----

main : Program () Model Api.ApiClient.Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


---- MODEL ----

type alias Model = { response : Response, hovering: List Api.ApiClient.Build }
type alias SuccessData =
    { branches: List String, totalTime: Int, list: List Api.ApiClient.Build }
type alias Job =
    { id: Float, time: Time.Posix }

type Response = Initial
    | Loading
    | Failure
    | Success SuccessData


init : ( Model, Cmd Api.ApiClient.Msg )
init =
    ( initModel, Api.ApiClient.fetchBuilds )

initModel : Model
initModel = { response = Loading, hovering = [] }

---- UPDATE ----


update : Api.ApiClient.Msg -> Model -> ( Model, Cmd Api.ApiClient.Msg )
update msg model =
    case msg of
        Api.ApiClient.FetchAll -> ( { model | response = Loading }, Api.ApiClient.fetchBuilds)
        Api.ApiClient.FetchSuccessful -> ( { model | response = Loading }, Api.ApiClient.fetchSuccessfullJobs)
        Api.ApiClient.AllJobs data ->
            case data of
                Ok d -> ( { model | response = Success (parseBuildData d) }, Cmd.none )
                Err _ -> ( { model | response = Failure }, Cmd.none )
        Api.ApiClient.SuccessfullJobs data ->
            case data of
                Ok d -> ( { model | response = Success (parseBuildData d) }, Cmd.none )
                Err _ -> ( { model | response = Failure }, Cmd.none )
        Api.ApiClient.Hover hovering -> ( { model | hovering = hovering }, Cmd.none )

parseBuildData : List Api.ApiClient.Build -> SuccessData
parseBuildData data =
    { branches = parseBranches data , totalTime = parseTotalTime data, list = data }

parseBranches: List Api.ApiClient.Build -> List String
parseBranches data =
    data
        |> List.map (\b -> b.branch)
        |> Set.fromList
        |> Set.toList

parseTotalTime: List Api.ApiClient.Build -> Int
parseTotalTime data =
    data
        |> List.map (\d -> d.time)
        |> List.foldl (+) 0

---- VIEW ----


view : Model -> Html Api.ApiClient.Msg
view model =
    div []
        [
        button [ onClick Api.ApiClient.FetchAll ] [ text "Show all" ] ,
        button [ onClick Api.ApiClient.FetchSuccessful ] [ text "Show successful" ] ,
        render model.response model
        ]

render : Response -> Model -> Html Api.ApiClient.Msg
render res model =
    case res of
        Initial -> h1 [] [ text "Initial" ]
        Loading -> renderLoadingIcon
        Failure -> h1 [] [ text "Error" ]
        Success data -> div [class "metadata_window"] [
            h1 [class "metadata"] [ span [class "span"] [ text "Scheduled jobs: " ], text (formatJobsCount data.list) ] ,
            h1 [class "metadata"] [ span [class "span"] [ text "Total Runtime: " ], text (formatTime data.totalTime) ] ,
            h1 [class "metadata"] [ span [class "span"] [ text "Branches:" ] ] ,
            ul [class "metadata"] [ formatBranches data.branches ] ,
            renderChart model (orderByBuildNum data.list)
            ]

renderLoadingIcon : Html Api.ApiClient.Msg
renderLoadingIcon =
    div [class "loading"] [
        Loading.render BouncingBalls { defaultConfig | color = "#ff003d", size = 40 } Loading.On
    ]

orderByBuildNum : List Api.ApiClient.Build -> List Api.ApiClient.Build
orderByBuildNum list =
    list |> List.sortBy (\b -> b.num)

renderChart : Model -> List Api.ApiClient.Build -> Html.Html Api.ApiClient.Msg
renderChart model jobs =
    LineChart.viewCustom
        { y = customAxis
          , x = Axis.default 1750 "id" (toFloat << .num)
          , container = Container.default "line-chart-1"
          , interpolation = Interpolation.linear
          , intersection = Intersection.default
          , legends = Legends.default
          , events =
                Events.custom
                    [ Events.onMouseMove Api.ApiClient.Hover Events.getNearestX
                    , Events.onMouseLeave (Api.ApiClient.Hover [])
                    ]
          , junk = Junk.hoverMany model.hovering formatX formatY
          , grid = Grid.default
          , area = Area.default
          , line = Line.default
          , dots = Dots.hoverMany model.hovering
          }
        [ LineChart.line colorUndoRed Dots.circle "Successful" jobs ]

formatX : Api.ApiClient.Build -> String
formatX build =
    "id: #" ++ String.fromInt build.num

formatY : Api.ApiClient.Build -> String
formatY build =
    build.time
        |> Time.millisToPosix
        |> formatMinutes

formatMinutes : Time.Posix -> String
formatMinutes posix =
    "finished in: "
    ++
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

formatJobsCount : List Api.ApiClient.Build -> String
formatJobsCount jobs =
    jobs
        |> List.length
        |> String.fromInt

customAxis : Axis.Config Api.ApiClient.Build msg
customAxis =
  Axis.custom
    { title = Title.default "minutes"
    --, variable = Just << (Duration.inMinutes << Duration.milliseconds << toFloat << Time.posixToMillis << .time)
    , variable = Just << (Duration.inMinutes << Duration.milliseconds << toFloat << .time)
    , pixels = 750
    , range = Range.padded 5 20
    , axisLine = AxisLine.rangeFrame Color.gray
    , ticks =
        Ticks.float 20
        -- Ticks.timeCustom Time.utc 20 customTimeTick
    }

customTimeTick : Tick.Time -> Tick.Config msg
customTimeTick info =
  let
    label =
      Tick.format info
      -- customFormat info
      -- customFormat2 info
  in
  Tick.custom
    { position = toFloat (Time.posixToMillis info.timestamp)
    , color = Color.black
    , width = 2
    , length = 8
    , grid = False
    , direction = Tick.negative
    , label = Just <| Junk.label Color.black label
    }