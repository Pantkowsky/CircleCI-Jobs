module Models exposing (Build, JobData, Msg(..), parseData)

import Http
import Set
import Time

type alias Build =
    {
    branch: String,
    num: Int,
    time: Int,
    status: String
    }

type alias JobData =
    {
    branches: List String,
    totalTime: Int,
    jobs: List Build
    }

type Msg = Data (Result Http.Error (List Build))
    | Hover (List Build)

parseData : (List String, Int, List Build) -> List Build -> JobData
parseData (branches, totalTime, jobs) data =
    { branches = branches ++ parseBranches data , totalTime = totalTime + parseTotalTime data, jobs = jobs ++ data }

parseBranches: List Build -> List String
parseBranches data =
    data
        |> List.map (\b -> b.branch)
        |> Set.fromList
        |> Set.toList

parseTotalTime: List Build -> Int
parseTotalTime data =
    data
        |> List.map (\d -> d.time)
        |> List.foldl (+) 0