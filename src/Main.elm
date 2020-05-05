module Main exposing (..)

import Browser
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html exposing (..)
import Api.ApiClient
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

type alias Model = { response : Response }
type alias SuccessData =
    { branches: List String, totalTime: Int, list: List Api.ApiClient.Build }

type Response = Initial
    | Loading
    | Failure
    | Success SuccessData


init : ( Model, Cmd Api.ApiClient.Msg )
init =
    ( initModel, Api.ApiClient.fetchBuilds )

initModel : Model
initModel = { response = Loading }

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
        render model.response
        ]

render : Response -> Html Api.ApiClient.Msg
render res =
    case res of
        Initial -> h1 [] [ text "Initial" ]
        Loading -> renderLoadingIcon
        Failure -> h1 [] [ text "Error" ]
        Success data -> div [class "metadata_window"] [
            h1 [class "metadata"] [ span [class "span"] [ text "Scheduled jobs: " ], text (formatJobsCount data.list) ] ,
            h1 [class "metadata"] [ span [class "span"] [ text "Total Runtime: " ], text (formatTime data.totalTime) ] ,
            h1 [class "metadata"] [ span [class "span"] [ text "Branches:" ] ] ,
            ul [class "metadata"] [ formatBranches data.branches ]
            ]

renderLoadingIcon : Html Api.ApiClient.Msg
renderLoadingIcon =
    div [class "loading"] [
        Loading.render BouncingBalls { defaultConfig | color = "#ff003d", size = 40 } Loading.On
    ]

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
