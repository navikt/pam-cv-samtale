module Jobbprofil.Kompetanse exposing (..)

import Json.Decode exposing (Decoder, at, int, map, map2, map3, string)
import Json.Encode


type Kompetanse
    = Kompetanse KompetanseInfo


type alias KompetanseInfo =
    { konseptid : Int
    , label : String
    }


fraEnkeltElementer : String -> Int -> Kompetanse
fraEnkeltElementer label_ konsept =
    Kompetanse
        { konseptid = konsept
        , label = label_
        }


label : Kompetanse -> String
label (Kompetanse info) =
    info.label


konseptId : Kompetanse -> Int
konseptId (Kompetanse info) =
    info.konseptid


decode : Decoder Kompetanse
decode =
    decodeBackendData
        |> map Kompetanse


decodeBackendData : Decoder KompetanseInfo
decodeBackendData =
    map2 KompetanseInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)


encode : Kompetanse -> Json.Encode.Value
encode (Kompetanse info) =
    Json.Encode.object
        [ ( "konseptid", Json.Encode.int info.konseptid )
        , ( "tittel", Json.Encode.string info.label )
        ]
