module ApiClient exposing (..)

import Endpoints exposing (..)
import Models exposing (Job, Msg(..))
import Json.Decode as Decoder exposing (Decoder, field, int, map4, string)
import Http

requestData : Cmd Msg
requestData =
    endpoints
            |> List.map fetchSuccessfulJobs
            |> Cmd.batch

fetchSuccessfulJobs : String -> Cmd Msg
fetchSuccessfulJobs endpoint =
    Http.get
        {
        url = endpoint
        , expect = Http.expectJson Data successfulJobsDecoder
        }

successfulJobsDecoder : Decoder (List Job)
successfulJobsDecoder =
    Decoder.list buildDecoder
        |> Decoder.map (List.filter isStatusSuccess)

buildDecoder : Decoder Job
buildDecoder =
    map4 Job
        (field "branch" string)
        (field "build_num" int)
        (field "build_time_millis" int)
        (field "status" string)

isStatusSuccess : Job -> Bool
isStatusSuccess build =
    build.status == "success"