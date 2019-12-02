module Fagdokumentasjon.Konsept exposing
    ( Konsept
    , decode
    , konseptId
    , label
    )

import FrontendModuler.Knapp as Knapp
import Html exposing (Html)
import Json.Decode exposing (Decoder, at, int, map, map2, string)


type Konsept
    = Konsept KonseptTypeaheadInfo


type alias KonseptTypeaheadInfo =
    { konseptId : Int
    , label : String
    }


label : Konsept -> String
label (Konsept info) =
    info.label


konseptId : Konsept -> Int
konseptId (Konsept info) =
    info.konseptId


decode : Decoder Konsept
decode =
    decodeBackendData
        |> map Konsept


decodeBackendData : Decoder KonseptTypeaheadInfo
decodeBackendData =
    map2 KonseptTypeaheadInfo
        (at [ "konseptId" ] int)
        (at [ "label" ] string)
