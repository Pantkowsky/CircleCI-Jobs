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

fetchAll : Cmd Msg
fetchAll =
    Cmd.batch [
        fetchAllJobs modularisationUrl,
        fetchAllJobs analyticsUrl,
        fetchAllJobs coreUrl,
        fetchAllJobs utilUrl,
        fetchAllJobs utilUrl,
        fetchAllJobs cleanupsUrl,
        fetchAllJobs deeplinksUrl,
        fetchAllJobs dependenciesUrl,
        fetchAllJobs formattersUrl,
        fetchAllJobs validatorsUrl,
        fetchAllJobs disposeByUrl,
        fetchAllJobs miscUrl,
        fetchAllJobs serviceManagerUrl,
        fetchAllJobs sessionUrl,
        fetchSuccessfulJobs modelsUrl,
        fetchSuccessfulJobs pricingUrl,
        fetchSuccessfulJobs widgetsUrl,
        fetchSuccessfulJobs errorsUrl,
        fetchSuccessfulJobs resourcesUrl
    ]

fetchSuccessful : Cmd Msg
fetchSuccessful =
    Cmd.batch [
        fetchSuccessfulJobs modularisationUrl,
        fetchSuccessfulJobs analyticsUrl,
        fetchSuccessfulJobs coreUrl,
        fetchSuccessfulJobs utilUrl,
        fetchSuccessfulJobs utilUrl,
        fetchSuccessfulJobs cleanupsUrl,
        fetchSuccessfulJobs deeplinksUrl,
        fetchSuccessfulJobs dependenciesUrl,
        fetchSuccessfulJobs formattersUrl,
        fetchSuccessfulJobs validatorsUrl,
        fetchSuccessfulJobs disposeByUrl,
        fetchSuccessfulJobs miscUrl,
        fetchSuccessfulJobs serviceManagerUrl,
        fetchSuccessfulJobs sessionUrl,
        fetchSuccessfulJobs modelsUrl,
        fetchSuccessfulJobs pricingUrl,
        fetchSuccessfulJobs widgetsUrl,
        fetchSuccessfulJobs errorsUrl,
        fetchSuccessfulJobs resourcesUrl
    ]

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

modularisationUrl : String
modularisationUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization", appendBuildSuffix]

analyticsUrl : String
analyticsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-analytics", appendBuildSuffix]

coreUrl : String
coreUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-core", appendBuildSuffix]

serviceManagerUrl : String
serviceManagerUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-service-manager", appendBuildSuffix]

sessionUrl : String
sessionUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-session", appendBuildSuffix]

miscUrl : String
miscUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-misc", appendBuildSuffix]

deeplinksUrl : String
deeplinksUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-deeplinks", appendBuildSuffix]

cleanupsUrl : String
cleanupsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-cleanups", appendBuildSuffix]

disposeByUrl : String
disposeByUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-disposeBy-removal", appendBuildSuffix]

dependenciesUrl : String
dependenciesUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-dependencies", appendBuildSuffix]

formattersUrl : String
formattersUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-formatters", appendBuildSuffix]

validatorsUrl : String
validatorsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-validators", appendBuildSuffix]

utilUrl : String
utilUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-util", appendBuildSuffix]

modelsUrl : String
modelsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-models", appendBuildSuffix]

pricingUrl : String
pricingUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-pricing", appendBuildSuffix]

resourcesUrl : String
resourcesUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-resources", appendBuildSuffix]

widgetsUrl : String
widgetsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-widgets", appendBuildSuffix]

errorsUrl : String
errorsUrl = String.concat["https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-errors", appendBuildSuffix]

appendApiToken : String
appendApiToken = String.concat["?circle-token=", tokenCircleCI]

appendBuildSuffix : String
appendBuildSuffix = String.concat[appendApiToken, "&limit=50"]

isStatusSuccess : Build -> Bool
isStatusSuccess build =
    build.status == "success"