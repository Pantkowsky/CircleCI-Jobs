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

type Msg = SuccessfullJobs (Result Http.Error (List Build))
    | Hover (List Build)

fetchSuccessful : Cmd Msg
fetchSuccessful =
    endpoints
            |> List.map fetchSuccessfulJobs
            |> Cmd.batch

fetchSuccessfulJobs : String -> Cmd Msg
fetchSuccessfulJobs endpoint =
    Http.get
        {
        url = endpoint
        , expect = Http.expectJson SuccessfullJobs successfulJobsDecoder
        }

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