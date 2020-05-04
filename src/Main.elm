module Main exposing (..)

import Browser
import Html.Attributes exposing (class)
import Html exposing (..)
import Api.ApiClient
import Task

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
    { id: Int, name: String, login: String, email: String, list: List Api.ApiClient.Build }

type Response = Initial
    | Loading
    | Failure
    | Success SuccessData


init : ( Model, Cmd Api.ApiClient.Msg )
init =
    ( initModel, fetchInitial )

initModel : Model
initModel = Model Initial

---- UPDATE ----


fetchInitial : Cmd Api.ApiClient.Msg
fetchInitial =
    Task.perform (always Api.ApiClient.Fetch) (Task.succeed ())

update : Api.ApiClient.Msg -> Model -> ( Model, Cmd Api.ApiClient.Msg )
update msg model =
    case msg of
        Api.ApiClient.Fetch -> ( { model | response = Loading }, Api.ApiClient.fetchData )
        Api.ApiClient.MeData data ->
            case data of
                Ok d -> ( { model | response = Success (parseSuccessData d) }, Api.ApiClient.fetchBuilds )
                Err _ -> ( { model | response = Failure }, Cmd.none )
        Api.ApiClient.BuildData data ->
            case data of
                Ok d -> ( { model | response = Success (parseBuildData d) }, Cmd.none )
                Err _ -> ( { model | response = Failure }, Cmd.none )

parseSuccessData : Api.ApiClient.Me -> SuccessData
parseSuccessData data =
    { id = data.githubId, name = data.name, login = data.login, email = data.email, list = [] }

parseBuildData : List Api.ApiClient.Build -> SuccessData
parseBuildData data =
    { id = 1, name = "name", login = "login", email = "email", list = data }

---- VIEW ----


view : Model -> Html Api.ApiClient.Msg
view model =
    div []
        [ render model.response ]

render : Response -> Html Api.ApiClient.Msg
render res =
    case res of
        Initial -> h1 [] [ text "Initial" ]
        Loading -> h1 [] [ text "Loading" ]
        Failure -> h1 [] [ text "Error" ]
        Success data -> div [class "metadata_window"] [
            h1 [class "metadata"] [ text (formatId data.id) ]
            , h1 [class "metadata"] [ text (formatName data.name) ]
            , h1 [class "metadata"] [ text (formatLogin data.login) ]
            , h1 [class "metadata"] [ text (formatEmail data.email) ]
            , h1 [class "metadata"] [ text (formatBuildNum data.list) ]
            ]

formatLogin : String -> String
formatLogin login =
    String.concat["login: ", login]

formatEmail : String -> String
formatEmail email =
    String.concat["email: ", email]

formatName : String -> String
formatName name =
    String.concat["name: ", name]

formatId : Int -> String
formatId id =
    String.concat["github_id: ", String.fromInt id]

formatBuildNum : List Api.ApiClient.Build -> String
formatBuildNum data =
    String.concat["jobs: ", filterBuilds data]

filterBuilds : List Api.ApiClient.Build -> String
filterBuilds builds =
    List.filter isStatusSuccess builds
        |> List.length
        |> String.fromInt

isStatusSuccess : Api.ApiClient.Build -> Bool
isStatusSuccess build =
    build.status == "success"



