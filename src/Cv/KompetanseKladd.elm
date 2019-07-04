module Cv.KompetanseKladd exposing (KompetanseKladd, beskrivelse, decode, id, konseptId, tittel)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type KompetanseKladd
    = KompetanseKladd KompetanseKladdInfo


type alias KompetanseKladdInfo =
    { id : String
    , tittel : Maybe String
    , konseptId : Maybe Int
    , beskrivelse : Maybe String
    }


id : KompetanseKladd -> String
id (KompetanseKladd info) =
    info.id


beskrivelse : KompetanseKladd -> Maybe String
beskrivelse (KompetanseKladd info) =
    info.beskrivelse


tittel : KompetanseKladd -> Maybe String
tittel (KompetanseKladd info) =
    info.tittel


konseptId : KompetanseKladd -> Maybe Int
konseptId (KompetanseKladd info) =
    info.konseptId



---- Decoder ----


decode : Decoder KompetanseKladd
decode =
    decodeBackendData
        |> map KompetanseKladd


decodeBackendData : Decoder KompetanseKladdInfo
decodeBackendData =
    succeed KompetanseKladdInfo
        |> required "id" string
        |> required "tittel" (nullable string)
        |> required "konseptid" (nullable int)
        -- OBS KONSEPTID ER EGENTLIG EN LONG.
        |> required "beskrivelse" (nullable string)
