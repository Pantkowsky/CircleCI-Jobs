module Api.ApiClient exposing (..)

import Json.Decode as JD exposing (Decoder, field, int, map4, string)
import Http
import Secrets exposing (tokenCircleCI)

type alias Build =
    {
    branch: String,
    num: Int,
    time: Int,
    status: String
    }

type Msg = FetchAll
    | FetchSuccessful
    | AllJobs (Result Http.Error (List Build))
    | SuccessfullJobs (Result Http.Error (List Build))
    | Hover (List Build)

fetchBuilds : Cmd Msg
fetchBuilds =
    Http.get
    {
    url = buildsUrl
    , expect = Http.expectJson AllJobs buildListDecoder
    }

fetchSuccessfullJobs : Cmd Msg
fetchSuccessfullJobs =
    Http.get
    {
    url = buildsUrl
    , expect = Http.expectJson SuccessfullJobs successfulJobsDecoder
    }

buildListDecoder : Decoder (List Build)
buildListDecoder =
    JD.list buildDecoder

successfulJobsDecoder : Decoder (List Build)
successfulJobsDecoder =
    JD.list buildDecoder
        |> JD.map (List.filter isStatusSuccess)

buildDecoder : Decoder Build
buildDecoder =
    map4 Build
        (field "branch" string)
        (field "build_num" int)
        (field "build_time_millis" int)
        (field "status" string)

buildsUrl : String
buildsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization", appendBuildSuffix]

appendApiToken : String
appendApiToken = String.concat["?circle-token=", tokenCircleCI]

appendBuildSuffix : String
appendBuildSuffix = String.concat[appendApiToken, "&limit=50"]

isStatusSuccess : Build -> Bool
isStatusSuccess build =
    build.status == "success"