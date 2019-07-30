module Yrke exposing
    ( Yrke
    , YrkeTypeaheadInfo
    , decode
    , konseptId
    , label
    , styrkkode
    )

import FrontendModuler.Knapp as Knapp
import Html exposing (Html)
import Json.Decode exposing (Decoder, at, int, map, map3, string)


type Yrke
    = Yrke YrkeTypeaheadInfo


type alias YrkeTypeaheadInfo =
    { konseptId : Int
    , label : String
    , styrk08 : String
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


decodeBackendData : Decoder YrkeTypeaheadInfo
decodeBackendData =
    map3 YrkeTypeaheadInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
        (at [ "styrk08" ] string)