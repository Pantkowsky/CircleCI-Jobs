module Main exposing (..)

import Browser
import Secrets exposing (tokenCircleCI)
import Html exposing (div, img, button, Html, text, h1)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, map, field, string)

---- MODEL ----


type alias Model = {response : Response}
type alias Me = { login: String }

type Response = Initial
    | Loading
    | Failure
    | Success String


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )

initModel : Model
initModel = Model Initial

---- UPDATE ----


type Msg = NoOp
    | Fetch
    | ApiData (Result Http.Error Me)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( { model | response = Initial }, Cmd.none )
        Fetch -> ( { model | response = Loading }, fetchData )
        ApiData data ->
            case data of
                Ok d -> ( { model | response = Success d.login }, Cmd.none )
                Err _ -> ( { model | response = Failure }, Cmd.none )

fetchData : Cmd Msg
fetchData =
    Http.get
        { url = requestUrl
        , expect = Http.expectJson ApiData decoder}


decoder : Decoder Me
decoder =
    map Me
        (field "login" string)

requestUrl : String
requestUrl = String.concat["https://circleci.com/api/v1.1/me", apiToken]

apiToken : String
apiToken = String.concat["?circle-token=", tokenCircleCI]

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , button [ onClick Fetch ] [ text "Fetch data" ]
        , me model.response
        ]

me : Response -> Html Msg
me res =
    case res of
        Initial -> h1 [] [ text "Initial" ]
        Loading -> h1 [] [ text "Loading" ]
        Failure -> h1 [] [ text "Error" ]
        Success s -> h1 [] [ text s]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
