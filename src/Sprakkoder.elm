module Sprakkoder exposing (Sprakkoder(..), SprakkoderInfo, decode, kode, term)

import Json.Decode exposing (Decoder, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)


type Sprakkoder
    = Sprakkoder SprakkoderInfo


type alias SprakkoderInfo =
    { kode : String
    , term : String
    }


kode : Sprakkoder -> String
kode (Sprakkoder info) =
    info.kode


term : Sprakkoder -> String
term (Sprakkoder info) =
    info.term



-- DECODER --


decode : Decoder Sprakkoder
decode =
    decodeBackendData
        |> map Sprakkoder


decodeBackendData : Decoder SprakkoderInfo
decodeBackendData =
    succeed SprakkoderInfo
        |> required "kode" string
        |> required "term" string
