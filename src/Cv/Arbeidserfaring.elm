module Cv.Arbeidserfaring exposing
    ( Arbeidserfaring
    , arbeidsgiver
    , beskrivelse
    , decode
    , fradato
    , id
    , ikkeAktueltForFremtiden
    , konseptid
    , navarende
    , sted
    , styrkkode
    , tildato
    , yrke
    , yrkeFritekst
    )

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Arbeidserfaring
    = Arbeidserfaring ArbeidserfaringInfo


type AAregArbeidserfating
    = AAregArbeidserfaring (List ArbeidserfaringInfo)


type alias ArbeidserfaringInfo =
    { id : String
    , arbeidsgiver : Maybe String
    , yrke : Maybe String
    , sted : Maybe String
    , fradato : Maybe String
    , tildato : Maybe String
    , navarende : Bool
    , styrkkode : Maybe String
    , ikkeAktueltForFremtiden : Bool
    , yrkeFritekst : Maybe String
    , konseptid : Maybe String
    , beskrivelse : Maybe String
    }


id : Arbeidserfaring -> String
id (Arbeidserfaring info) =
    info.id


arbeidsgiver : Arbeidserfaring -> Maybe String
arbeidsgiver (Arbeidserfaring info) =
    info.arbeidsgiver


yrke : Arbeidserfaring -> Maybe String
yrke (Arbeidserfaring info) =
    info.yrke


sted : Arbeidserfaring -> Maybe String
sted (Arbeidserfaring info) =
    info.sted


fradato : Arbeidserfaring -> Maybe String
fradato (Arbeidserfaring info) =
    info.fradato


tildato : Arbeidserfaring -> Maybe String
tildato (Arbeidserfaring info) =
    info.tildato


navarende : Arbeidserfaring -> Bool
navarende (Arbeidserfaring info) =
    info.navarende


styrkkode : Arbeidserfaring -> Maybe String
styrkkode (Arbeidserfaring info) =
    info.styrkkode


ikkeAktueltForFremtiden : Arbeidserfaring -> Bool
ikkeAktueltForFremtiden (Arbeidserfaring info) =
    info.ikkeAktueltForFremtiden


yrkeFritekst : Arbeidserfaring -> Maybe String
yrkeFritekst (Arbeidserfaring info) =
    info.yrkeFritekst


konseptid : Arbeidserfaring -> Maybe String
konseptid (Arbeidserfaring info) =
    info.konseptid


beskrivelse : Arbeidserfaring -> Maybe String
beskrivelse (Arbeidserfaring info) =
    info.beskrivelse



{--
Arbeidserfaringdto:

 id: String?,
 arbeidsgiver: String?,
 yrke: String?,
 sted: String?,
 fradato: String?,
 tildato: String?,
 navarende: Boolean,
 styrkkode: String?,
 ikkeAktueltForFremtiden: Boolean,
 yrkeFritekst: String?,
 konseptid: String?,
 beskrivelse: String?
)
--}
---- Decoder ----


decode : Decoder Arbeidserfaring
decode =
    decodeBackendData
        |> map Arbeidserfaring


decodeBackendData : Decoder ArbeidserfaringInfo
decodeBackendData =
    succeed ArbeidserfaringInfo
        |> required "id" string
        |> required "arbeidsgiver" (nullable string)
        |> required "yrke" (nullable string)
        |> required "sted" (nullable string)
        |> required "fradato" (nullable string)
        |> required "tildato" (nullable string)
        |> required "navarende" bool
        |> required "styrkkode" (nullable string)
        |> required "ikkeAktueltForFremtiden" bool
        |> required "yrkeFritekst" (nullable string)
        |> required "konseptid" (nullable string)
        |> required "beskrivelse" (nullable string)
