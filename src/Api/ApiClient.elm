module ApiClient exposing (..)

import Endpoints exposing (..)
import Models exposing (Build, Msg(..))
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

successfulJobsDecoder : Decoder (List Build)
successfulJobsDecoder =
    Decoder.list buildDecoder
        |> Decoder.map (List.filter isStatusSuccess)

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