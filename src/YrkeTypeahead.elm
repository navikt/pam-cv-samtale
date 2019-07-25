module YrkeTypeahead exposing (YrkeTypeahead(..), YrkeTypeaheadInfo, decode, konseptId, label, styrkkode)

import FrontendModuler.Knapp as Knapp
import Html exposing (Html)
import Json.Decode exposing (Decoder, at, int, map, map3, string)


type YrkeTypeahead
    = YrkeTypeahead YrkeTypeaheadInfo


type alias YrkeTypeaheadInfo =
    { konseptId : Int
    , label : String
    , styrk08 : String
    }


label : YrkeTypeahead -> String
label (YrkeTypeahead info) =
    info.label


styrkkode : YrkeTypeahead -> String
styrkkode (YrkeTypeahead info) =
    info.styrk08


konseptId : YrkeTypeahead -> Int
konseptId (YrkeTypeahead info) =
    info.konseptId


decode : Decoder YrkeTypeahead
decode =
    decodeBackendData
        |> map YrkeTypeahead


decodeBackendData : Decoder YrkeTypeaheadInfo
decodeBackendData =
    map3 YrkeTypeaheadInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
        (at [ "styrk08" ] string)
