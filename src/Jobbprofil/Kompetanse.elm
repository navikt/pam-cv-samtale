module Jobbprofil.Kompetanse exposing (..)

import Json.Decode exposing (Decoder, at, int, map, map2, map3, string)


type Kompetanse
    = Kompetanse KompetanseInfo


type alias KompetanseInfo =
    { konseptid : Int
    , label : String
    }


label : Kompetanse -> String
label (Kompetanse info) =
    info.label


decode : Decoder Kompetanse
decode =
    decodeBackendData
        |> map Kompetanse


decodeBackendData : Decoder KompetanseInfo
decodeBackendData =
    map2 KompetanseInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
