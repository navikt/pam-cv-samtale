module Sertifikat.Sertifikat exposing
    ( Sertifikat
    , decode
    , fraDato
    , id
    , konseptId
    , sertifikatNavn
    , sertifikatNavnFritekst
    , sistEndretDato
    , tilDato
    , utsteder
    )

import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Posix)


type Sertifikat
    = Sertifikat SertifikatInfo


type alias SertifikatInfo =
    { id : String
    , sertifikatNavnFritekst : Maybe String
    , sertifikatnavn : Maybe String
    , konseptId : Maybe String
    , utsteder : Maybe String
    , fradato : Maybe String
    , tildato : Maybe String
    , sistEndretDato : Posix
    }


id : Sertifikat -> String
id (Sertifikat info) =
    info.id


sertifikatNavnFritekst : Sertifikat -> Maybe String
sertifikatNavnFritekst (Sertifikat info) =
    info.sertifikatNavnFritekst


sertifikatNavn : Sertifikat -> Maybe String
sertifikatNavn (Sertifikat info) =
    info.sertifikatnavn


konseptId : Sertifikat -> Maybe String
konseptId (Sertifikat info) =
    info.konseptId


fraDato : Sertifikat -> Maybe String
fraDato (Sertifikat info) =
    info.fradato


tilDato : Sertifikat -> Maybe String
tilDato (Sertifikat info) =
    info.tildato


utsteder : Sertifikat -> Maybe String
utsteder (Sertifikat info) =
    info.utsteder


sistEndretDato : Sertifikat -> Posix
sistEndretDato (Sertifikat info) =
    info.sistEndretDato



---- Decoder ----


decode : Decoder Sertifikat
decode =
    decodeBackendData
        |> map Sertifikat


decodeBackendData : Decoder SertifikatInfo
decodeBackendData =
    succeed SertifikatInfo
        |> required "id" string
        |> required "sertifikatnavnFritekst" (nullable string)
        |> required "sertifikatnavn" (nullable string)
        |> required "konseptId" (nullable string)
        |> required "utsteder" (nullable string)
        |> required "fradato" (nullable string)
        |> required "tildato" (nullable string)
        |> required "sistEndretDato" Iso8601.decoder
