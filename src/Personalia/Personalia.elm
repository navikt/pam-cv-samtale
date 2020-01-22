module Personalia.Personalia exposing
    ( Personalia
    , decode
    , epost
    , etternavn
    , fodselsdato
    , fornavn
    , gateadresse
    , id
    , postnummer
    , poststed
    , sistEndretDato
    , telefon
    )

import Iso8601
import Json.Decode exposing (Decoder, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Personalia.PersonaliaId as PersonaliaId exposing (PersonaliaId)
import Time exposing (Posix)


type Personalia
    = Personalia PersonaliaInfo


type alias PersonaliaInfo =
    { id : PersonaliaId
    , fornavn : Maybe String
    , etternavn : Maybe String
    , epost : Maybe String
    , telefon : Maybe String
    , fodselsdato : Maybe String
    , gateadresse : Maybe String
    , postnummer : Maybe String
    , poststed : Maybe String
    , sistEndretDato : Posix
    }


id : Personalia -> PersonaliaId
id (Personalia info) =
    info.id


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


sistEndretDato : Personalia -> Posix
sistEndretDato (Personalia info) =
    info.sistEndretDato



--- DECODER ---


decode : Decoder Personalia
decode =
    decodeBackendData
        |> Json.Decode.map Personalia


decodeBackendData : Decoder PersonaliaInfo
decodeBackendData =
    succeed PersonaliaInfo
        |> required "id" PersonaliaId.decode
        |> required "fornavn" (nullable string)
        |> required "etternavn" (nullable string)
        |> required "epost" (nullable string)
        |> required "telefon" (nullable string)
        |> required "fodselsdato" (nullable string)
        |> required "gateadresse" (nullable string)
        |> required "postnummer" (nullable string)
        |> required "poststed" (nullable string)
        |> required "sistEndretDato" Iso8601.decoder
