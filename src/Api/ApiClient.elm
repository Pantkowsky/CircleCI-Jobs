module Api.ApiClient exposing (..)

import Json.Decode as JD exposing (Decoder, field, map4, string, int)
import Http
import Secrets exposing (tokenCircleCI)

type alias Build =
    {
    branch: String,
    num: Int,
    time: Int,
    status: String
    }

type Msg = Fetch
    | BuildData (Result Http.Error (List Build))

fetchBuilds : Cmd Msg
fetchBuilds =
    Http.get
    {
    url = buildsUrl
    , expect = Http.expectJson BuildData buildListDecoder
    }


buildListDecoder : Decoder (List Build)
buildListDecoder =
    JD.list buildDecoder

buildDecoder : Decoder Build
buildDecoder =
    map4 Build
        (field "branch" string)
        (field "build_num" int)
        (field "build_time_millis" int)
        (field "status" string)

requestUrl : String
requestUrl = String.concat["https://circleci.com/api/v1.1/me", appendApiToken]

buildsUrl : String
buildsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization", appendBuildSuffix]

appendApiToken : String
appendApiToken = String.concat["?circle-token=", tokenCircleCI]

appendBuildSuffix : String
appendBuildSuffix = String.concat[appendApiToken, "&limit=50"]