module Api.ApiClient exposing (..)

import Json.Decode as JD exposing (Decoder, field, map, map2, map4, string, int)
import Http
import Secrets exposing (tokenCircleCI)

type alias Me =
    {
    githubId: Int,
    name: String,
    login: String,
    email: String
    }

type alias Build =
    {
    num: Int,
    time: Int
    }

type Msg = Fetch
    | MeData (Result Http.Error Me)
    | BuildData (Result Http.Error (List Build))

fetchData : Cmd Msg
fetchData =
    Http.get
        { url = requestUrl
        , expect = Http.expectJson MeData decoder}

fetchBuilds : Cmd Msg
fetchBuilds =
    Http.get
    {
    url = buildsUrl
    , expect = Http.expectJson BuildData buildListDecoder
    }


decoder : Decoder Me
decoder =
    map4 Me
        (field "github_id" int)
        (field "name" string)
        (field "login" string)
        (field "selected_email" string)

buildListDecoder : Decoder (List Build)
buildListDecoder =
    JD.list buildDecoder

buildDecoder : Decoder Build
buildDecoder =
    map2 Build
        (field "build_num" int)
        (field "build_time_millis" int)

requestUrl : String
requestUrl = String.concat["https://circleci.com/api/v1.1/me", appendApiToken]

buildsUrl : String
buildsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization", appendBuildSuffix]

appendApiToken : String
appendApiToken = String.concat["?circle-token=", tokenCircleCI]

appendBuildSuffix : String
appendBuildSuffix = String.concat[appendApiToken, "&limit=50"]