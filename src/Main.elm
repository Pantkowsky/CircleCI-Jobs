module Main exposing (..)

import Browser
import Charts exposing (renderChart)
import Models exposing (Job, Msg(..), JobData, parseData)
import Html.Attributes exposing (class)
import Html exposing (..)
import ApiClient exposing (requestData)
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

type alias Model = { response : Response, data: JobData, hovering: List Job }

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

asJobData : Model -> List Job -> JobData
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
        Success ->
            let jobs = model.data.jobs
                totalTime = model.data.totalTime
                branches = model.data.branches
                hovered = model.hovering
            in
            div [] [
                h1 [class "metadata"] [ span [class "span"] [ text "Scheduled jobs: " ], text (formatJobsCount jobs) ] ,
                h1 [class "metadata"] [ span [class "span"] [ text "Total Runtime: " ], text (formatTime totalTime) ] ,
                h1 [class "metadata"] [ span [class "span"] [ text "Branches:" ] ] ,
                ul [class "metadata"] [ formatBranches branches ] ,
                renderChart hovered (orderByBuildNum jobs)
            ]

orderByBuildNum : List Job -> List Job
orderByBuildNum list =
    list |> List.sortBy (\b -> b.num)

renderLoadingIcon : Html Msg
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

formatJobsCount : List Job -> String
formatJobsCount jobs =
    jobs
        |> List.length
        |> String.fromInt