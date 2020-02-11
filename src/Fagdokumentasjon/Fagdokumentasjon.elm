module Fagdokumentasjon.Fagdokumentasjon exposing
    ( Fagdokumentasjon
    , FagdokumentasjonType(..)
    , decode
    , fagdokumentasjonType
    , id
    , konseptId
    , sistEndretDato
    , tittel
    )

import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Posix)


type Fagdokumentasjon
    = Fagdokumentasjon FagdokumentasjonInfo


type alias FagdokumentasjonInfo =
    { id : String
    , tittel : Maybe String
    , konseptId : Maybe String
    , fagdokumentasjonType : FagdokumentasjonType
    , sistEndretDato : Posix
    }


type FagdokumentasjonType
    = SvennebrevFagbrev
    | Mesterbrev
    | Autorisasjon


id : Fagdokumentasjon -> String
id (Fagdokumentasjon info) =
    info.id


tittel : Fagdokumentasjon -> Maybe String
tittel (Fagdokumentasjon info) =
    info.tittel


konseptId : Fagdokumentasjon -> Maybe String
konseptId (Fagdokumentasjon info) =
    info.konseptId


fagdokumentasjonType : Fagdokumentasjon -> FagdokumentasjonType
fagdokumentasjonType (Fagdokumentasjon info) =
    info.fagdokumentasjonType


sistEndretDato : Fagdokumentasjon -> Posix
sistEndretDato (Fagdokumentasjon info) =
    info.sistEndretDato



---- Decoder ----


decode : Decoder Fagdokumentasjon
decode =
    decodeBackendData
        |> andThen tilFagdokumentasjonInfo
        |> map Fagdokumentasjon


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "id" string
        |> required "tittel" (nullable string)
        |> required "konseptId" (nullable string)
        |> required "type" string
        |> required "sistEndretDato" Iso8601.decoder


tilFagdokumentasjonInfo : BackendData -> Decoder FagdokumentasjonInfo
tilFagdokumentasjonInfo backendData =
    decodeFagdokument backendData.type_
        |> map (lagFagdokumentasjonInfo backendData)


lagFagdokumentasjonInfo : BackendData -> FagdokumentasjonType -> FagdokumentasjonInfo
lagFagdokumentasjonInfo backendData fd =
    { id = backendData.id
    , tittel = backendData.tittel
    , konseptId = backendData.konseptId
    , fagdokumentasjonType = fd
    , sistEndretDato = backendData.sistEndretDato
    }


decodeFagdokument : String -> Decoder FagdokumentasjonType
decodeFagdokument fd =
    if fd == "SVENNEBREV_FAGBREV" then
        succeed SvennebrevFagbrev

    else if fd == "MESTERBREV" then
        succeed Mesterbrev

    else if fd == "AUTORISASJON" then
        succeed Autorisasjon

    else
        fail ("Decoding av enum Fagdokumentype feilet. Klarer ikke decode verdi: " ++ fd)


type alias BackendData =
    { id : String
    , tittel : Maybe String
    , konseptId : Maybe String
    , type_ : String
    , sistEndretDato : Posix
    }
