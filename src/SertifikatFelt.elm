module SertifikatFelt exposing
    ( SertifikatFelt
    , decode
    , konseptId
    , label
    )

import Json.Decode exposing (Decoder, at, int, map, map2, string)


type SertifikatFelt
    = SertifikatFelt SertifikatFeltTypeaheadInfo


type alias SertifikatFeltTypeaheadInfo =
    { konseptId : Int
    , label : String
    }


label : SertifikatFelt -> String
label (SertifikatFelt info) =
    info.label


konseptId : SertifikatFelt -> Int
konseptId (SertifikatFelt info) =
    info.konseptId


decode : Decoder SertifikatFelt
decode =
    decodeBackendData
        |> map SertifikatFelt


decodeBackendData : Decoder SertifikatFeltTypeaheadInfo
decodeBackendData =
    map2 SertifikatFeltTypeaheadInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
