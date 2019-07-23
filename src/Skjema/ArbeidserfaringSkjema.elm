module Skjema.ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema(..), Felt(..), SkjemaInfo, arbeidsoppgaver, bedriftNavn, jobbTittel, lokasjon, naavarende, oppdaterStringFelt, setBool, yrke)

import Dato exposing (Dato)


type ArbeidserfaringSkjema
    = ArbeidserfaringSkjema SkjemaInfo


type alias SkjemaInfo =
    { yrke : String
    , jobbTittel : String
    , bedriftNavn : String
    , lokasjon : String
    , arbeidsoppgaver : String
    , fraDato : Dato
    , naavarende : Bool
    , tilDato : Maybe Dato
    }


type Felt
    = Yrke
    | JobbTittel
    | BedriftNavn
    | Lokasjon
    | Arbeidsoppgaver
    | FraDato
    | Naavarende
    | TilDato


yrke : ArbeidserfaringSkjema -> String
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



--- OPPDATER FELT ETTER EGENSKAP ----


oppdaterStringFelt : ArbeidserfaringSkjema -> Felt -> Maybe String -> Maybe Bool -> ArbeidserfaringSkjema
oppdaterStringFelt skjema felt maybeString maybeBool =
    case ( maybeBool, maybeString ) of
        ( Just boolean, Nothing ) ->
            setBool skjema felt boolean

        ( Nothing, Just string ) ->
            case felt of
                Yrke ->
                    string
                        |> oppdaterYrkeFelt skjema

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

                _ ->
                    skjema

        _ ->
            skjema


setBool : ArbeidserfaringSkjema -> Felt -> Bool -> ArbeidserfaringSkjema
setBool (ArbeidserfaringSkjema skjema) felt bool =
    case felt of
        Naavarende ->
            ArbeidserfaringSkjema { skjema | naavarende = bool }

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
