module Cv.Sammendrag exposing (Sammendrag, decode, sammendrag)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Sammendrag
    = Sammendrag SammendragInfo


type alias SammendragInfo =
    { sammendrag : String
    }


sammendrag : Sammendrag -> String
sammendrag (Sammendrag info) =
    info.sammendrag



---- Decoder ----


decode : Decoder Sammendrag
decode =
    decodeBackendData
        |> map Sammendrag


decodeBackendData : Decoder SammendragInfo
decodeBackendData =
    succeed SammendragInfo
        |> required "sammendrag" string
