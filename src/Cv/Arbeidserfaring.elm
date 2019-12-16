module Cv.Arbeidserfaring exposing
    ( Arbeidserfaring
    , arbeidsgiver
    , beskrivelse
    , decode
    , fraMåned
    , fraÅr
    , id
    , sted
    , tilDato
    , yrke
    , yrkeFritekst
    , yrkeString
    )

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Dato exposing (TilDato(..), År)
import Dato.Maned exposing (Måned)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Arbeidserfaring
    = Arbeidserfaring ArbeidserfaringInfo


type AAregArbeidserfaring
    = AAregArbeidserfaring (List ArbeidserfaringInfo)


type alias ArbeidserfaringInfo =
    { id : String
    , arbeidsgiver : Maybe String
    , yrke : Maybe Yrke
    , sted : Maybe String
    , fraMåned : Måned
    , fraÅr : År
    , tilDato : TilDato
    , yrkeFritekst : Maybe String
    , beskrivelse : Maybe String
    }


id : Arbeidserfaring -> String
id (Arbeidserfaring info) =
    info.id


arbeidsgiver : Arbeidserfaring -> Maybe String
arbeidsgiver (Arbeidserfaring info) =
    info.arbeidsgiver


yrke : Arbeidserfaring -> Maybe Yrke
yrke (Arbeidserfaring info) =
    info.yrke


yrkeString : Arbeidserfaring -> Maybe String
yrkeString (Arbeidserfaring info) =
    Maybe.map Yrke.label info.yrke


sted : Arbeidserfaring -> Maybe String
sted (Arbeidserfaring info) =
    info.sted


fraMåned : Arbeidserfaring -> Måned
fraMåned (Arbeidserfaring info) =
    info.fraMåned


fraÅr : Arbeidserfaring -> År
fraÅr (Arbeidserfaring info) =
    info.fraÅr


tilDato : Arbeidserfaring -> TilDato
tilDato (Arbeidserfaring info) =
    info.tilDato


yrkeFritekst : Arbeidserfaring -> Maybe String
yrkeFritekst (Arbeidserfaring info) =
    info.yrkeFritekst


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
        |> andThen tilArbeidserfaring


tilArbeidserfaring : BackendData -> Decoder Arbeidserfaring
tilArbeidserfaring backendData =
    Json.Decode.map2 (lagArbeidserfaring backendData)
        (Dato.decodeMonthYear backendData.fradato)
        (decodeTilDato backendData.navarende backendData.tildato)


lagArbeidserfaring : BackendData -> ( Måned, År ) -> TilDato -> Arbeidserfaring
lagArbeidserfaring backendData ( fraMåned_, fraÅr_ ) tilDato_ =
    Arbeidserfaring
        { id = backendData.id
        , arbeidsgiver = backendData.arbeidsgiver
        , sted = backendData.sted
        , fraMåned = fraMåned_
        , fraÅr = fraÅr_
        , tilDato = tilDato_
        , yrkeFritekst = backendData.yrkeFritekst
        , beskrivelse = backendData.beskrivelse
        , yrke =
            Maybe.map3 Yrke.fraString
                backendData.yrke
                backendData.styrkkode
                backendData.konseptid
        }


decodeTilDato : Bool -> Maybe String -> Decoder TilDato
decodeTilDato nåværende maybeTilDatoString =
    if nåværende then
        succeed Nåværende

    else
        case maybeTilDatoString of
            Just tilDatoString ->
                tilDatoString
                    |> Dato.decodeMonthYear
                    |> Json.Decode.map (\( a, b ) -> Avsluttet a b)

            Nothing ->
                fail "Decoding av til-dato feilet. nåværende kan ikke være false samtidig som tildato er null"


type alias BackendData =
    { id : String
    , arbeidsgiver : Maybe String
    , yrke : Maybe String
    , sted : Maybe String
    , fradato : String
    , tildato : Maybe String
    , navarende : Bool
    , styrkkode : Maybe String
    , yrkeFritekst : Maybe String
    , konseptid : Maybe String
    , beskrivelse : Maybe String
    }


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "id" string
        |> required "arbeidsgiver" (nullable string)
        |> required "yrke" (nullable string)
        |> required "sted" (nullable string)
        |> required "fradato" string
        |> required "tildato" (nullable string)
        |> required "navarende" bool
        |> required "styrkkode" (nullable string)
        |> required "yrkeFritekst" (nullable string)
        |> required "konseptid" (nullable string)
        |> required "beskrivelse" (nullable string)
