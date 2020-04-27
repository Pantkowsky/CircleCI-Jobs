module Main exposing (..)

import Browser
import Html.Attributes exposing (class)
import Secrets exposing (tokenCircleCI)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, map2, field, string)

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

type alias Model = { response : Response }
type alias Me = { login: String, email: String }

type Response = Initial
    | Loading
    | Failure
    | Success String String


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )

initModel : Model
initModel = Model Initial

---- UPDATE ----

type Msg = Fetch
    | ApiData (Result Http.Error Me)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch -> ( { model | response = Loading }, fetchData )
        ApiData data ->
            case data of
                Ok d -> ( { model | response = Success d.login d.email }, Cmd.none )
                Err _ -> ( { model | response = Failure }, Cmd.none )

fetchData : Cmd Msg
fetchData =
    Http.get
        { url = requestUrl
        , expect = Http.expectJson ApiData decoder}


decoder : Decoder Me
decoder =
    map2 Me
        (field "login" string)
        (field "selected_email" string)

requestUrl : String
requestUrl = String.concat["https://circleci.com/api/v1.1/me", apiToken]

apiToken : String
apiToken = String.concat["?circle-token=", tokenCircleCI]

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [
        button [ onClick Fetch ] [ text "Fetch data" ]
        , me model.response
        ]

me : Response -> Html Msg
me res =
    case res of
        Initial -> h1 [] [ text "Initial" ]
        Loading -> h1 [] [ text "Loading" ]
        Failure -> h1 [] [ text "Error" ]
        Success l e -> li [class "metadata_window"] [
            h1 [class "metadata"] [ text (formatLogin l) ]
            , h1 [class "metadata"] [text (formatEmail e)]
            ]

formatLogin : String -> String
formatLogin login =
    String.concat["login: ", login]

formatEmail : String -> String
formatEmail email =
    String.concat["email: ", email]



