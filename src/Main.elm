module Main exposing (..)

import Browser
import Html.Attributes exposing (class)
import Html exposing (..)
import Html.Events exposing (onClick)
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

type Response = Initial
    | Loading
    | Failure
    | Success Int String String String


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
        Api.ApiClient.ApiData data ->
            case data of
                Ok d -> ( { model | response = Success d.githubId d.name d.login d.email }, Cmd.none )
                Err _ -> ( { model | response = Failure }, Cmd.none )

---- VIEW ----


view : Model -> Html Api.ApiClient.Msg
view model =
    div []
        [ me model.response ]

me : Response -> Html Api.ApiClient.Msg
me res =
    case res of
        Initial -> h1 [] [ text "Initial" ]
        Loading -> h1 [] [ text "Loading" ]
        Failure -> h1 [] [ text "Error" ]
        Success i n l e -> li [class "metadata_window"] [
            h1 [class "metadata"] [ text (formatId i) ]
            , h1 [class "metadata"] [text (formatName n)]
            , h1 [class "metadata"] [text (formatLogin l)]
            , h1 [class "metadata"] [text (formatEmail e)]
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



