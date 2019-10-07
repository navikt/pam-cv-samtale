module SertifikatTypeahead exposing
    ( SertifikatTypeahead
    , decode
    , konseptId
    , label
    )

import Json.Decode exposing (Decoder, at, int, map, map2, string)


type SertifikatTypeahead
    = SertifikatTypeahead SertifikatFeltTypeaheadInfo


type alias SertifikatFeltTypeaheadInfo =
    { konseptId : Int
    , label : String
    }


label : SertifikatTypeahead -> String
label (SertifikatTypeahead info) =
    info.label


konseptId : SertifikatTypeahead -> Int
konseptId (SertifikatTypeahead info) =
    info.konseptId


decode : Decoder SertifikatTypeahead
decode =
    decodeBackendData
        |> map SertifikatTypeahead


decodeBackendData : Decoder SertifikatFeltTypeaheadInfo
decodeBackendData =
    map2 SertifikatFeltTypeaheadInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
