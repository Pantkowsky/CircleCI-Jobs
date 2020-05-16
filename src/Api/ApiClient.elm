module Api.ApiClient exposing (..)

import Api.Endpoints exposing (..)
import Json.Decode as JD exposing (Decoder, field, int, map4, string)
import Http

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

fetchAll : Cmd Msg
fetchAll =
    endpoints
        |> List.map fetchAllJobs
        |> Cmd.batch

fetchSuccessful : Cmd Msg
fetchSuccessful =
    endpoints
            |> List.map fetchSuccessfulJobs
            |> Cmd.batch

fetchAllJobs : String -> Cmd Msg
fetchAllJobs endpoint =
    Http.get
        {
        url = endpoint
        , expect = Http.expectJson AllJobs buildListDecoder
        }

fetchSuccessfulJobs : String -> Cmd Msg
fetchSuccessfulJobs endpoint =
    Http.get
        {
        url = endpoint
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

isStatusSuccess : Build -> Bool
isStatusSuccess build =
    build.status == "success"