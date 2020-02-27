module Jobbprofil.Omrade exposing (..)

import Json.Decode exposing (Decoder, at, int, map, map3, string)
import Json.Encode


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


konseptId : Omrade -> Int
konseptId (Omrade info) =
    info.konseptId


fraEnkeltElementer : String -> Int -> String -> Omrade
fraEnkeltElementer label konsept kode =
    Omrade
        { konseptId = konsept
        , tittel = label
        , kode = kode
        }


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


encode : Omrade -> Json.Encode.Value
encode (Omrade info) =
    Json.Encode.object
        [ ( "konseptid", Json.Encode.int info.konseptId )
        , ( "tittel", Json.Encode.string info.tittel )
        , ( "kode", Json.Encode.string info.kode )
        ]
