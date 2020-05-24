module Models exposing (Job, JobData, Msg(..), parseData)

import Http
import Set

type alias Job =
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
    jobs: List Job
    }

type Msg = Data (Result Http.Error (List Job))
    | Hover (List Job)

parseData : (List String, Int, List Job) -> List Job -> JobData
parseData (branches, totalTime, jobs) data =
    { branches = branches ++ parseBranches data , totalTime = totalTime + parseTotalTime data, jobs = jobs ++ data }

parseBranches: List Job -> List String
parseBranches data =
    data
        |> List.map (\b -> b.branch)
        |> Set.fromList
        |> Set.toList

parseTotalTime: List Job -> Int
parseTotalTime data =
    data
        |> List.map (\d -> d.time)
        |> List.foldl (+) 0