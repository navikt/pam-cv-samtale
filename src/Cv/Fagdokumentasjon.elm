module Cv.Fagdokumentasjon exposing (Fagdokumentasjon, beskrivelse, decode, id, konseptId, tittel, type_)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Fagdokumentasjon
    = Fagdokumentasjon FagdokumentasjonInfo


type alias FagdokumentasjonInfo =
    { id : String
    , tittel : Maybe String
    , beskrivelse : Maybe String
    , konseptId : Maybe String
    , type_ : Fagdokumentype
    }


type Fagdokumentype
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


beskrivelse : Fagdokumentasjon -> Maybe String
beskrivelse (Fagdokumentasjon info) =
    info.beskrivelse


type_ : Fagdokumentasjon -> Fagdokumentype
type_ (Fagdokumentasjon info) =
    info.type_



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
        |> required "beskrivelse" (nullable string)
        |> required "konseptId" (nullable string)
        |> required "type" string


tilFagdokumentasjonInfo : BackendData -> Decoder FagdokumentasjonInfo
tilFagdokumentasjonInfo backendData =
    decodeFagdokument backendData.type_
        |> map (lagFagdokumentasjonInfo backendData)


lagFagdokumentasjonInfo : BackendData -> Fagdokumentype -> FagdokumentasjonInfo
lagFagdokumentasjonInfo backendData fd =
    { id = backendData.id
    , tittel = backendData.tittel
    , konseptId = backendData.konseptId
    , beskrivelse = backendData.beskrivelse
    , type_ = fd
    }


decodeFagdokument : String -> Decoder Fagdokumentype
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
    , beskrivelse : Maybe String
    , type_ : String
    }
