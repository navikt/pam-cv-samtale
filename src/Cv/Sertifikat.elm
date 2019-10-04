module Cv.Sertifikat exposing
    ( Sertifikat
    , decode
    , fradato
    , id
    , konseptId
    , sertifikatNavnFritekst
    , sertifikatnavn
    , tildato
    , utsteder
    )

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


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
    }


id : Sertifikat -> String
id (Sertifikat info) =
    info.id


sertifikatNavnFritekst : Sertifikat -> Maybe String
sertifikatNavnFritekst (Sertifikat info) =
    info.sertifikatNavnFritekst


sertifikatnavn : Sertifikat -> Maybe String
sertifikatnavn (Sertifikat info) =
    info.sertifikatnavn


konseptId : Sertifikat -> Maybe String
konseptId (Sertifikat info) =
    info.konseptId


fradato : Sertifikat -> Maybe String
fradato (Sertifikat info) =
    info.fradato


tildato : Sertifikat -> Maybe String
tildato (Sertifikat info) =
    info.tildato


utsteder : Sertifikat -> Maybe String
utsteder (Sertifikat info) =
    info.utsteder



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
