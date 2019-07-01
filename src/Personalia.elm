module Personalia exposing
    ( Personalia
    , decode
    , epost
    , etternavn
    , fodselsdato
    , fornavn
    , gateadresse
    , postnummer
    , poststed
    , telefon
    )

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Personalia
    = Personalia PersonaliaInfo


type alias PersonaliaInfo =
    { fornavn : Maybe String
    , etternavn : Maybe String
    , epost : Maybe String
    , telefon : Maybe String
    , fodselsdato : Maybe String
    , gateadresse : Maybe String
    , postnummer : Maybe String
    , poststed : Maybe String
    }


fornavn : Personalia -> Maybe String
fornavn (Personalia info) =
    info.fornavn


etternavn : Personalia -> Maybe String
etternavn (Personalia info) =
    info.etternavn


epost : Personalia -> Maybe String
epost (Personalia info) =
    info.epost


telefon : Personalia -> Maybe String
telefon (Personalia info) =
    info.telefon


fodselsdato : Personalia -> Maybe String
fodselsdato (Personalia info) =
    info.fodselsdato


gateadresse : Personalia -> Maybe String
gateadresse (Personalia info) =
    info.gateadresse


postnummer : Personalia -> Maybe String
postnummer (Personalia info) =
    info.postnummer


poststed : Personalia -> Maybe String
poststed (Personalia info) =
    info.poststed



--- DECODER ---


decode : Decoder Personalia
decode =
    decodeBackendData
        |> map Personalia


decodeBackendData : Decoder PersonaliaInfo
decodeBackendData =
    succeed PersonaliaInfo
        |> required "fornavn" (nullable string)
        |> required "etternavn" (nullable string)
        |> required "epost" (nullable string)
        |> required "telefon" (nullable string)
        |> required "fodselsdato" (nullable string)
        |> required "gateadresse" (nullable string)
        |> required "postnummer" (nullable string)
        |> required "poststed" (nullable string)
