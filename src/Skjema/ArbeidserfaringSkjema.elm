module Skjema.ArbeidserfaringSkjema exposing
    ( ArbeidserfaringSkjema(..)
    , Felt(..)
    , SkjemaInfo
    , TypeaheadFelt(..)
    , ValidertArbeidserfaringSkjema
    , arbeidsoppgaver
    , bedriftNavn
    , encode
    , fraDato
    , jobbTittel
    , lokasjon
    , mapTypeaheadState
    , naavarende
    , nyttValidertSkjema
    , oppdaterStringFelt
    , oppdaterYrkeFelt
    , setYrkeFeltTilYrke
    , tilArbeidserfaringSkjema
    , tilDato
    , toggleBool
    , valider
    , velgAktivYrkeITypeahead
    , yrke
    , yrkeTypeahead
    )

import Dato exposing (Dato)
import Json.Encode
import TypeaheadState exposing (TypeaheadState)
import Yrke exposing (Yrke)


type ArbeidserfaringSkjema
    = ArbeidserfaringSkjema SkjemaInfo


type ValidertArbeidserfaringSkjema
    = ValidertArbeidserfaringSkjema ValidertSkjemaInfo


type alias ValidertSkjemaInfo =
    { yrke : Yrke
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


type TypeaheadFelt
    = Yrke Yrke
    | Typeahead (TypeaheadState Yrke)


type alias SkjemaInfo =
    { yrke : TypeaheadFelt
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
    = JobbTittel
    | BedriftNavn
    | Lokasjon
    | Arbeidsoppgaver
    | FraMåned
    | FraÅr
    | Naavarende
    | TilMåned
    | TilÅr


yrkeTypeahead : ArbeidserfaringSkjema -> TypeaheadFelt
yrkeTypeahead (ArbeidserfaringSkjema info) =
    info.yrke


yrke : ValidertArbeidserfaringSkjema -> Yrke
yrke (ValidertArbeidserfaringSkjema info) =
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


oppdaterYrkeFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterYrkeFelt (ArbeidserfaringSkjema skjema) string =
    case skjema.yrke of
        Yrke _ ->
            ArbeidserfaringSkjema
                { skjema
                    | yrke =
                        TypeaheadState.init string
                            |> Typeahead
                }

        Typeahead typeaheadState ->
            ArbeidserfaringSkjema
                { skjema
                    | yrke =
                        typeaheadState
                            |> TypeaheadState.updateValue string
                            |> Typeahead
                }


mapTypeaheadState : ArbeidserfaringSkjema -> (TypeaheadState Yrke -> TypeaheadState Yrke) -> ArbeidserfaringSkjema
mapTypeaheadState (ArbeidserfaringSkjema skjema) funksjon =
    case skjema.yrke of
        Yrke _ ->
            ArbeidserfaringSkjema
                skjema

        Typeahead typeaheadState ->
            ArbeidserfaringSkjema
                { skjema
                    | yrke =
                        typeaheadState
                            |> funksjon
                            |> Typeahead
                }


velgAktivYrkeITypeahead : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
velgAktivYrkeITypeahead (ArbeidserfaringSkjema info) =
    case info.yrke of
        Yrke _ ->
            ArbeidserfaringSkjema info

        Typeahead typeaheadState ->
            case TypeaheadState.getActive typeaheadState of
                Just active ->
                    ArbeidserfaringSkjema { info | yrke = Yrke active }

                Nothing ->
                    ArbeidserfaringSkjema info


setYrkeFeltTilYrke : Yrke -> ArbeidserfaringSkjema -> ArbeidserfaringSkjema
setYrkeFeltTilYrke yrke_ (ArbeidserfaringSkjema info) =
    ArbeidserfaringSkjema { info | yrke = Yrke yrke_ }


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


nyttValidertSkjema : ValidertSkjemaInfo -> ValidertArbeidserfaringSkjema
nyttValidertSkjema skjemaInfo =
    ValidertArbeidserfaringSkjema skjemaInfo


tilArbeidserfaringSkjema : ValidertArbeidserfaringSkjema -> ArbeidserfaringSkjema
tilArbeidserfaringSkjema (ValidertArbeidserfaringSkjema info) =
    ArbeidserfaringSkjema
        { yrke = Yrke info.yrke
        , jobbTittel = info.jobbTittel
        , bedriftNavn = info.bedriftNavn
        , lokasjon = info.lokasjon
        , arbeidsoppgaver = info.arbeidsoppgaver
        , fraDato = info.fraDato
        , naavarende = info.naavarende
        , tilDato = info.tilDato
        , styrkkode = info.styrkkode
        , konseptId = info.konseptId
        }


valider : ArbeidserfaringSkjema -> Maybe ValidertArbeidserfaringSkjema
valider (ArbeidserfaringSkjema info) =
    case info.yrke of
        Yrke yrkefelt ->
            ValidertArbeidserfaringSkjema
                { yrke = yrkefelt
                , jobbTittel = info.jobbTittel
                , bedriftNavn = info.bedriftNavn
                , lokasjon = info.lokasjon
                , arbeidsoppgaver = info.arbeidsoppgaver
                , fraDato = info.fraDato
                , naavarende = info.naavarende
                , tilDato = info.tilDato
                , styrkkode = info.styrkkode
                , konseptId = info.konseptId
                }
                |> Just

        Typeahead typeaheadState ->
            Nothing


encode : ValidertArbeidserfaringSkjema -> Json.Encode.Value
encode (ValidertArbeidserfaringSkjema skjema) =
    case skjema.tilDato of
        Just tilArbeidsDato ->
            Json.Encode.object
                [ ( "arbeidsgiver", Json.Encode.string skjema.bedriftNavn )
                , ( "yrke", Json.Encode.string (Yrke.label skjema.yrke) )
                , ( "sted", Json.Encode.string skjema.lokasjon )
                , ( "fradato", Json.Encode.string (skjema.fraDato |> Dato.tilStringForBackend) )
                , ( "tildato", Json.Encode.string (tilArbeidsDato |> Dato.tilStringForBackend) )
                , ( "navarende", Json.Encode.bool skjema.naavarende )
                , ( "yrkeFritekst", Json.Encode.string skjema.jobbTittel )
                , ( "beskrivelse", Json.Encode.string skjema.arbeidsoppgaver )
                , ( "styrkkode", Json.Encode.string skjema.styrkkode )
                , ( "konseptid", Json.Encode.int skjema.konseptId )
                ]

        Nothing ->
            Json.Encode.object
                [ ( "arbeidsgiver", Json.Encode.string skjema.bedriftNavn )
                , ( "yrke", Json.Encode.string (Yrke.label skjema.yrke) )
                , ( "sted", Json.Encode.string skjema.lokasjon )
                , ( "fradato", Json.Encode.string (skjema.fraDato |> Dato.tilStringForBackend) )
                , ( "navarende", Json.Encode.bool skjema.naavarende )
                , ( "yrkeFritekst", Json.Encode.string skjema.jobbTittel )
                , ( "beskrivelse", Json.Encode.string skjema.arbeidsoppgaver )
                , ( "styrkkode", Json.Encode.string skjema.styrkkode )
                , ( "konseptid", Json.Encode.int skjema.konseptId )
                ]
