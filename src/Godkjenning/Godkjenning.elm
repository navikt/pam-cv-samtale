module Godkjenning.Godkjenning exposing
    ( Godkjenning
    , decode
    , fraDato
    , godkjenningTittel
    , id
    , konseptId
    , sistEndretDato
    , tilDato
    , utsteder
    )

import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Posix)


type Godkjenning
    = Godkjenning GodkjenningInfo


type alias GodkjenningInfo =
    { id : String
    , tittel : Maybe String
    , konseptId : Maybe String
    , utsteder : Maybe String
    , fradato : Maybe String
    , tildato : Maybe String
    , sistEndretDato : Posix
    }


id : Godkjenning -> String
id (Godkjenning info) =
    info.id


godkjenningTittel : Godkjenning -> Maybe String
godkjenningTittel (Godkjenning info) =
    info.tittel


konseptId : Godkjenning -> Maybe String
konseptId (Godkjenning info) =
    info.konseptId


fraDato : Godkjenning -> Maybe String
fraDato (Godkjenning info) =
    info.fradato


tilDato : Godkjenning -> Maybe String
tilDato (Godkjenning info) =
    info.tildato


utsteder : Godkjenning -> Maybe String
utsteder (Godkjenning info) =
    info.utsteder


sistEndretDato : Godkjenning -> Posix
sistEndretDato (Godkjenning info) =
    info.sistEndretDato



---- Decoder ----


decode : Decoder Godkjenning
decode =
    decodeBackendData
        |> map Godkjenning


decodeBackendData : Decoder GodkjenningInfo
decodeBackendData =
    succeed GodkjenningInfo
        |> required "id" string
        |> required "tittel" (nullable string)
        |> required "konseptId" (nullable string)
        |> required "utsteder" (nullable string)
        |> required "fradato" (nullable string)
        |> required "tildato" (nullable string)
        |> required "sistEndretDato" Iso8601.decoder
