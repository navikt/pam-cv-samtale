module Cv.Sammendrag exposing (Sammendrag, decode, toString)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Sammendrag
    = Sammendrag String


toString : Sammendrag -> String
toString (Sammendrag sammendrag_) =
    sammendrag_



---- DECODER ----


decode : Decoder Sammendrag
decode =
    decodeBackendData
        |> map (.sammendrag >> Sammendrag)


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "sammendrag" string


type alias BackendData =
    { sammendrag : String
    }
