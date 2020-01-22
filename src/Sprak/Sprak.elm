module Sprak.Sprak exposing (Språk, decode, id, muntlig, sistEndretDato, skriftlig, sprak)

import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Posix)


type Språk
    = Språk SpråkInfo


type alias SpråkInfo =
    { id : String
    , sprak : Maybe String
    , muntlig : Maybe String
    , skriftlig : Maybe String
    , sistEndretDato : Posix
    }


id : Språk -> String
id (Språk info) =
    info.id


sprak : Språk -> Maybe String
sprak (Språk info) =
    info.sprak


muntlig : Språk -> Maybe String
muntlig (Språk info) =
    info.muntlig


skriftlig : Språk -> Maybe String
skriftlig (Språk info) =
    info.skriftlig


sistEndretDato : Språk -> Posix
sistEndretDato (Språk info) =
    info.sistEndretDato



---- Decoder ----


decode : Decoder Språk
decode =
    decodeBackendData
        |> map Språk


decodeBackendData : Decoder SpråkInfo
decodeBackendData =
    succeed SpråkInfo
        |> required "id" string
        |> required "sprak" (nullable string)
        |> required "muntlig" (nullable string)
        |> required "skriftlig" (nullable string)
        |> required "sistEndretDato" Iso8601.decoder
