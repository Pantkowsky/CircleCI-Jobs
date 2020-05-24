module Models exposing (..)

import Http

type alias Build =
    {
    branch: String,
    num: Int,
    time: Int,
    status: String
    }

type Msg = Data (Result Http.Error (List Build))
    | Hover (List Build)