module Jobbprofil.Omrade exposing (..)

import Json.Decode exposing (Decoder, at, int, map, map3, string)


type Omrade
    = Omrade OmradeInfo


type alias OmradeInfo =
    { konseptId : Int
    , tittel : String
    , kode : String
    }


tittel : Omrade -> String
tittel (Omrade info) =
    info.tittel


decode : Decoder Omrade
decode =
    decodeBackendData
        |> map Omrade


decodeBackendData : Decoder OmradeInfo
decodeBackendData =
    map3 OmradeInfo
        (at [ "konseptid" ] int)
        (at [ "tittel" ] string)
        (at [ "kode" ] string)
