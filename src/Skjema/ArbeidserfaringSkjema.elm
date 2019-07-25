module Skjema.ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema(..), Felt(..), SkjemaInfo, arbeidsoppgaver, bedriftNavn, encode, fraDato, jobbTittel, lokasjon, naavarende, oppdaterStringFelt, skjemaInfo, tilDato, toggleBool, yrke)

import Cv.Arbeidserfaring as Cv
import Dato exposing (Dato)
import Json.Encode
import TypeaheadState exposing (TypeaheadState)
import YrkeTypeahead exposing (YrkeTypeahead)


type ArbeidserfaringSkjema
    = ArbeidserfaringSkjema SkjemaInfo


type alias SkjemaInfo =
    { yrke : YrkeTypeahead
    , jobbTittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsoppgaver : String
    , fraDato : Dato
    , naavarende : Bool
    , tilDato : Maybe Dato
    , styrkkode : String
    , konseptId : Int
    }


type Felt
    = Yrke
    | JobbTittel
    | BedriftNavn
    | Lokasjon
    | Arbeidsoppgaver
    | FraMåned
    | FraÅr
    | Naavarende
    | TilMåned
    | TilÅr


skjemaInfo : ArbeidserfaringSkjema -> SkjemaInfo
skjemaInfo arbeidserfaringSkjema =
    case arbeidserfaringSkjema of
        ArbeidserfaringSkjema skjema ->
            skjema


yrke : ArbeidserfaringSkjema -> YrkeTypeahead
yrke (ArbeidserfaringSkjema info) =
    info.yrke


jobbTittel : ArbeidserfaringSkjema -> String
jobbTittel (ArbeidserfaringSkjema info) =
    info.jobbTittel


bedriftNavn : ArbeidserfaringSkjema -> String
bedriftNavn (ArbeidserfaringSkjema info) =
    info.bedriftNavn


lokasjon : ArbeidserfaringSkjema -> String
lokasjon (ArbeidserfaringSkjema info) =
    info.lokasjon


arbeidsoppgaver : ArbeidserfaringSkjema -> String
arbeidsoppgaver (ArbeidserfaringSkjema info) =
    info.arbeidsoppgaver


naavarende : ArbeidserfaringSkjema -> Bool
naavarende (ArbeidserfaringSkjema info) =
    info.naavarende


fraDato : ArbeidserfaringSkjema -> Dato
fraDato (ArbeidserfaringSkjema info) =
    info.fraDato


tilDato : ArbeidserfaringSkjema -> Maybe Dato
tilDato (ArbeidserfaringSkjema info) =
    info.tilDato



--- OPPDATER FELT ETTER EGENSKAP ----


oppdaterStringFelt : ArbeidserfaringSkjema -> Felt -> String -> ArbeidserfaringSkjema
oppdaterStringFelt skjema felt string =
    case felt of
        JobbTittel ->
            string
                |> oppdaterJobbTittelFelt skjema

        BedriftNavn ->
            string
                |> oppdaterBedriftNavnFelt skjema

        Lokasjon ->
            string
                |> oppdaterLokasjonFelt skjema

        Arbeidsoppgaver ->
            string
                |> oppdaterArbeidsoppgaverFelt skjema

        FraMåned ->
            string
                |> oppdaterFraMåned skjema

        FraÅr ->
            if string == "" then
                oppdaterFraÅr skjema string

            else
                case String.toInt string of
                    Just a ->
                        oppdaterFraÅr skjema string

                    Nothing ->
                        skjema

        TilMåned ->
            string
                |> oppdaterTilMåned skjema

        TilÅr ->
            string
                |> oppdaterTilÅr skjema

        _ ->
            skjema


toggleBool : ArbeidserfaringSkjema -> Felt -> ArbeidserfaringSkjema
toggleBool (ArbeidserfaringSkjema skjema) felt =
    case felt of
        Naavarende ->
            let
                nyVerdi =
                    skjema.naavarende
            in
            ArbeidserfaringSkjema { skjema | naavarende = not nyVerdi }

        _ ->
            ArbeidserfaringSkjema skjema



--- OPPDATER FELT ETTER NAVN ---


oppdaterNavarendeFelt : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
oppdaterNavarendeFelt (ArbeidserfaringSkjema skjema) =
    if skjema.naavarende == True then
        ArbeidserfaringSkjema { skjema | naavarende = False }

    else
        ArbeidserfaringSkjema { skjema | naavarende = True }


oppdaterYrkeFelt : ArbeidserfaringSkjema -> YrkeTypeahead -> ArbeidserfaringSkjema
oppdaterYrkeFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | yrke = string }


oppdaterJobbTittelFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterJobbTittelFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | jobbTittel = string }


oppdaterBedriftNavnFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterBedriftNavnFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | bedriftNavn = string }


oppdaterLokasjonFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterLokasjonFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | lokasjon = string }


oppdaterArbeidsoppgaverFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterArbeidsoppgaverFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | arbeidsoppgaver = string }


oppdaterFraMåned : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterFraMåned (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema
        { skjema
            | fraDato =
                string
                    |> Dato.stringTilMåned
                    |> Dato.setMåned skjema.fraDato
        }


oppdaterFraÅr : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterFraÅr (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema
        { skjema
            | fraDato =
                string
                    |> String.toInt
                    |> Maybe.withDefault 0
                    |> Dato.setÅr skjema.fraDato
        }


oppdaterTilMåned : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterTilMåned (ArbeidserfaringSkjema skjema) string =
    case skjema.tilDato of
        Just dato ->
            ArbeidserfaringSkjema
                { skjema
                    | tilDato =
                        string
                            |> Dato.stringTilMåned
                            |> Dato.setMåned dato
                            |> Just
                }

        Nothing ->
            ArbeidserfaringSkjema skjema


oppdaterTilÅr : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterTilÅr (ArbeidserfaringSkjema skjema) string =
    case skjema.tilDato of
        Just dato ->
            ArbeidserfaringSkjema
                { skjema
                    | tilDato =
                        string
                            |> String.toInt
                            |> Maybe.withDefault 0
                            |> Dato.setÅr dato
                            |> Just
                }

        Nothing ->
            ArbeidserfaringSkjema skjema


encode : ArbeidserfaringSkjema -> Json.Encode.Value
encode (ArbeidserfaringSkjema skjema) =
    case skjema.tilDato of
        Just tilArbeidsDato ->
            Json.Encode.object
                [ ( "arbeidsgiver", Json.Encode.string skjema.bedriftNavn )
                , ( "yrke", Json.Encode.string (YrkeTypeahead.label skjema.yrke) )
                , ( "sted", Json.Encode.string skjema.lokasjon )
                , ( "fradato", Json.Encode.string (skjema.fraDato |> Dato.tilStringForBackend) )
                , ( "tildato", Json.Encode.string (tilArbeidsDato |> Dato.tilStringForBackend) )
                , ( "navarende", Json.Encode.bool skjema.naavarende )
                , ( "yrkeFritekst", Json.Encode.string skjema.jobbTittel )
                , ( "beskrivelse", Json.Encode.string skjema.arbeidsoppgaver )
                , ( "styrkkode", Json.Encode.string skjema.styrkkode )
                , ( "konseptId", Json.Encode.int skjema.konseptId )
                ]

        Nothing ->
            Json.Encode.object
                [ ( "arbeidsgiver", Json.Encode.string skjema.bedriftNavn )
                , ( "yrke", Json.Encode.string (YrkeTypeahead.label skjema.yrke) )
                , ( "sted", Json.Encode.string skjema.lokasjon )
                , ( "fradato", Json.Encode.string (skjema.fraDato |> Dato.tilStringForBackend) )
                , ( "navarende", Json.Encode.bool skjema.naavarende )
                , ( "yrkeFritekst", Json.Encode.string skjema.jobbTittel )
                , ( "beskrivelse", Json.Encode.string skjema.arbeidsoppgaver )
                , ( "styrkkode", Json.Encode.string skjema.styrkkode )
                , ( "konseptId", Json.Encode.int skjema.konseptId )
                ]
