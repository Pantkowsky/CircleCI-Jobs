module Api.ApiClient exposing (..)

import Json.Decode exposing (Decoder, field, map4, string, int)
import Http
import Secrets exposing (tokenCircleCI)

type alias Me =
    {
    githubId: Int,
    name: String,
    login: String,
    email: String
    }

type Msg = Fetch
    | ApiData (Result Http.Error Me)

fetchData : Cmd Msg
fetchData =
    Http.get
        { url = requestUrl
        , expect = Http.expectJson ApiData decoder}


decoder : Decoder Me
decoder =
    map4 Me
        (field "github_id" int)
        (field "name" string)
        (field "login" string)
        (field "selected_email" string)

requestUrl : String
requestUrl = String.concat["https://circleci.com/api/v1.1/me", appendApiToken]

appendApiToken : String
appendApiToken = String.concat["?circle-token=", tokenCircleCI]