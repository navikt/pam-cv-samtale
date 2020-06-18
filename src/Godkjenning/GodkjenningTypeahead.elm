module Godkjenning.GodkjenningTypeahead exposing
    ( GodkjenningTypeahead
    , decode
    , konseptId
    , label
    )

import Json.Decode exposing (Decoder, at, int, map, map2, string)


type GodkjenningTypeahead
    = GodkjenningTypeahead GodkjenningFeltTypeaheadInfo


type alias GodkjenningFeltTypeaheadInfo =
    { konseptId : Int
    , label : String
    }


label : GodkjenningTypeahead -> String
label (GodkjenningTypeahead info) =
    info.label


konseptId : GodkjenningTypeahead -> Int
konseptId (GodkjenningTypeahead info) =
    info.konseptId


decode : Decoder GodkjenningTypeahead
decode =
    decodeBackendData
        |> map GodkjenningTypeahead


decodeBackendData : Decoder GodkjenningFeltTypeaheadInfo
decodeBackendData =
    map2 GodkjenningFeltTypeaheadInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
