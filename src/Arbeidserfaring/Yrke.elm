-- TODO: flytte fila siden den også brukes i jobbprofiL?


module Arbeidserfaring.Yrke exposing
    ( Yrke
    , decode
    , encode
    , fraString
    , konseptId
    , label
    , styrkkode
    )

import Json.Decode exposing (Decoder, at, int, map, map3, string)
import Json.Encode


type Yrke
    = Yrke YrkeInfo


type alias YrkeInfo =
    { konseptId : Int
    , label : String
    , styrk08 : String
    }


fraString : String -> String -> String -> Yrke
fraString yrk styrk konsept =
    Yrke
        { konseptId = Maybe.withDefault 1 (String.toInt konsept)
        , label = yrk
        , styrk08 = styrk
        }


label : Yrke -> String
label (Yrke info) =
    info.label


styrkkode : Yrke -> String
styrkkode (Yrke info) =
    info.styrk08


konseptId : Yrke -> Int
konseptId (Yrke info) =
    info.konseptId


decode : Decoder Yrke
decode =
    decodeBackendData
        |> map Yrke


decodeBackendData : Decoder YrkeInfo
decodeBackendData =
    map3 YrkeInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
        (at [ "styrk08" ] string)



--- ENCODE ---


encode : Yrke -> Json.Encode.Value
encode (Yrke info) =
    Json.Encode.object
        [ ( "styrk08", Json.Encode.string info.styrk08 )
        , ( "konseptid", Json.Encode.int info.konseptId )
        , ( "tittel", Json.Encode.string info.label )
        ]
