module Cv.Kurs exposing (Kurs, decode, id, tidspunkt, tittel, utsteder, varighet, varighetEnhet)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Kurs
    = Kurs KursInfo


type alias KursInfo =
    { id : String
    , tittel : Maybe String
    , utsteder : Maybe String
    , tidspunkt : Maybe String
    , varighet : Maybe Int
    , varighetEnhet : Maybe String
    }


id : Kurs -> String
id (Kurs info) =
    info.id


tidspunkt : Kurs -> Maybe String
tidspunkt (Kurs info) =
    info.tidspunkt


varighet : Kurs -> Maybe Int
varighet (Kurs info) =
    info.varighet


varighetEnhet : Kurs -> Maybe String
varighetEnhet (Kurs info) =
    info.varighetEnhet


tittel : Kurs -> Maybe String
tittel (Kurs info) =
    info.tittel


utsteder : Kurs -> Maybe String
utsteder (Kurs info) =
    info.utsteder



---- Decoder ----


decode : Decoder Kurs
decode =
    decodeBackendData
        |> map Kurs


decodeBackendData : Decoder KursInfo
decodeBackendData =
    succeed KursInfo
        |> required "id" string
        |> required "tittel" (nullable string)
        |> required "utsteder" (nullable string)
        |> required "tidspunkt" (nullable string)
        |> required "varighet" (nullable int)
        |> required "varighetEnhet" (nullable string)
