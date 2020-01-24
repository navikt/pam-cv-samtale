module Sammendrag exposing (Sammendrag, decode, encodeSammendrag, sistEndretDato, toString)

import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Time exposing (Posix)


type Sammendrag
    = Sammendrag { sammendrag : String, cvSistEndret : Posix }


toString : Sammendrag -> String
toString (Sammendrag info) =
    info.sammendrag


sistEndretDato : Sammendrag -> Posix
sistEndretDato (Sammendrag info) =
    info.cvSistEndret



--- ENCODE ---


encodeSammendrag : String -> Json.Encode.Value
encodeSammendrag sammendrag =
    Json.Encode.object [ ( "sammendrag", Json.Encode.string sammendrag ) ]



---- DECODER ----


decode : Decoder Sammendrag
decode =
    decodeBackendData
        |> map Sammendrag


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "sammendrag" string
        |> required "cvSistEndret" Iso8601.decoder


type alias BackendData =
    { sammendrag : String
    , cvSistEndret : Posix
    }
