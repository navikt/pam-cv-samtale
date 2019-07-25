module YrkeTypeahead exposing (YrkeTypeahead(..), YrkeTypeaheadInfo, decode, label)

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
