module Sammendrag exposing (Sammendrag, decode, encodeSammendrag, toString)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode


type Sammendrag
    = Sammendrag String


toString : Sammendrag -> String
toString (Sammendrag sammendrag_) =
    sammendrag_



--- ENCODE ---


encodeSammendrag : String -> Json.Encode.Value
encodeSammendrag sammendrag =
    Json.Encode.object [ ( "sammendrag", Json.Encode.string sammendrag ) ]



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
