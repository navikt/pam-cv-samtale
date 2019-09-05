module SpråkKode exposing (SpråkKode(..), SpråkKodeInfo, decode, engelsk, kode, norsk, term)

import Json.Decode exposing (Decoder, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)


type SpråkKode
    = SpråkKode SpråkKodeInfo


type alias SpråkKodeInfo =
    { kode : String
    , term : String
    }


kode : SpråkKode -> String
kode (SpråkKode info) =
    info.kode


term : SpråkKode -> String
term (SpråkKode info) =
    info.term


norsk : SpråkKode
norsk =
    SpråkKode
        { kode = "Norsk"
        , term = "Norsk"
        }


engelsk : SpråkKode
engelsk =
    SpråkKode
        { kode = "Engelsk"
        , term = "Engelsk"
        }



-- DECODER --


decode : Decoder SpråkKode
decode =
    decodeBackendData
        |> map SpråkKode


decodeBackendData : Decoder SpråkKodeInfo
decodeBackendData =
    succeed SpråkKodeInfo
        |> required "kode" string
        |> required "term" string
