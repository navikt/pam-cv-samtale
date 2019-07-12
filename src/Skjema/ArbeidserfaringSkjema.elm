module Skjema.ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema(..), Felt(..), SkjemaInfo, init, oppdaterStringFelt, sted, toggleBool, tomtSkjema, yrke, yrkeFritekst)

import Cv.Arbeidserfaring exposing (..)


type ArbeidserfaringSkjema
    = ArbeidserfaringSkjema SkjemaInfo


type alias SkjemaInfo =
    { id : String
    , arbeidsgiver : String
    , yrke : String
    , sted : String
    , fradato : String
    , tildato : String
    , navarende : Bool
    , styrkkode : String
    , ikkeAktueltForFremtiden : Bool
    , yrkeFritekst : String
    , konseptid : String
    , beskrivelse : String
    }


type Felt
    = Id
    | Arbeidsgiver
    | Yrke
    | Sted
    | Fradato
    | Tildato
    | Navarende
    | Styrkkode
    | IkkeAktueltForFremtiden
    | YrkeFritekst
    | Konseptid
    | Beskrivelse


yrke : ArbeidserfaringSkjema -> String
yrke (ArbeidserfaringSkjema info) =
    info.yrke


yrkeFritekst : ArbeidserfaringSkjema -> String
yrkeFritekst (ArbeidserfaringSkjema info) =
    info.yrkeFritekst


arbeidsgiver : ArbeidserfaringSkjema -> String
arbeidsgiver (ArbeidserfaringSkjema info) =
    info.arbeidsgiver


sted : ArbeidserfaringSkjema -> String
sted (ArbeidserfaringSkjema info) =
    info.sted


beskrivelse : ArbeidserfaringSkjema -> String
beskrivelse (ArbeidserfaringSkjema info) =
    info.beskrivelse


fradato : ArbeidserfaringSkjema -> String
fradato (ArbeidserfaringSkjema info) =
    info.fradato


tildato : ArbeidserfaringSkjema -> String
tildato (ArbeidserfaringSkjema info) =
    info.tildato


navarende : ArbeidserfaringSkjema -> Bool
navarende (ArbeidserfaringSkjema info) =
    info.navarende



--- OPPDATER FELT ETTER EGENSKAP ----


oppdaterStringFelt : ArbeidserfaringSkjema -> Felt -> String -> ArbeidserfaringSkjema
oppdaterStringFelt (ArbeidserfaringSkjema skjema) felt string =
    case felt of
        Id ->
            ArbeidserfaringSkjema { skjema | id = string }

        Arbeidsgiver ->
            ArbeidserfaringSkjema { skjema | arbeidsgiver = string }

        Yrke ->
            ArbeidserfaringSkjema { skjema | yrke = string }

        Sted ->
            ArbeidserfaringSkjema { skjema | sted = string }

        Fradato ->
            ArbeidserfaringSkjema { skjema | fradato = string }

        Tildato ->
            ArbeidserfaringSkjema { skjema | tildato = string }

        Styrkkode ->
            ArbeidserfaringSkjema { skjema | styrkkode = string }

        YrkeFritekst ->
            ArbeidserfaringSkjema { skjema | yrkeFritekst = string }

        Konseptid ->
            ArbeidserfaringSkjema { skjema | konseptid = string }

        Beskrivelse ->
            ArbeidserfaringSkjema { skjema | beskrivelse = string }

        _ ->
            ArbeidserfaringSkjema skjema


toggleBool : ArbeidserfaringSkjema -> Felt -> ArbeidserfaringSkjema
toggleBool skjema felt =
    case felt of
        Navarende ->
            oppdaterNavarendeFelt skjema

        IkkeAktueltForFremtiden ->
            oppdaterNavarendeFelt skjema

        _ ->
            skjema



--- OPPDATER FELT ETTER NAVN ---


oppdaterNavarendeFelt : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
oppdaterNavarendeFelt (ArbeidserfaringSkjema skjema) =
    if skjema.navarende == True then
        ArbeidserfaringSkjema { skjema | navarende = False }

    else
        ArbeidserfaringSkjema { skjema | navarende = True }


oppdaterIkkeAktueltForFremtidenFelt : ArbeidserfaringSkjema -> ArbeidserfaringSkjema
oppdaterIkkeAktueltForFremtidenFelt (ArbeidserfaringSkjema skjema) =
    if skjema.navarende == True then
        ArbeidserfaringSkjema { skjema | navarende = False }

    else
        ArbeidserfaringSkjema { skjema | navarende = True }


oppdaterIdFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterIdFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | id = string }


oppdaterArbeidsgiverFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterArbeidsgiverFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | arbeidsgiver = string }


oppdaterYrkeFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterYrkeFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | yrke = string }


oppdaterStedFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterStedFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | sted = string }


oppdaterFradatoFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterFradatoFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | fradato = string }


oppdaterTildatoFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterTildatoFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | tildato = string }


oppdaterStyrkkodeFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterStyrkkodeFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | styrkkode = string }


oppdaterYrkeFritekstFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterYrkeFritekstFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | yrkeFritekst = string }


oppdaterKonseptidFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterKonseptidFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | konseptid = string }


oppdaterBeskrivelseFelt : ArbeidserfaringSkjema -> String -> ArbeidserfaringSkjema
oppdaterBeskrivelseFelt (ArbeidserfaringSkjema skjema) string =
    ArbeidserfaringSkjema { skjema | beskrivelse = string }


init : Arbeidserfaring -> ArbeidserfaringSkjema
init arbeidserfaring =
    ArbeidserfaringSkjema
        { id =
            arbeidserfaring
                |> Cv.Arbeidserfaring.id
        , arbeidsgiver =
            arbeidserfaring
                |> Cv.Arbeidserfaring.arbeidsgiver
                |> Maybe.withDefault ""
        , yrke =
            arbeidserfaring
                |> Cv.Arbeidserfaring.yrke
                |> Maybe.withDefault ""
        , sted =
            arbeidserfaring
                |> Cv.Arbeidserfaring.sted
                |> Maybe.withDefault ""
        , fradato =
            arbeidserfaring
                |> Cv.Arbeidserfaring.fradato
                |> Maybe.withDefault ""
        , tildato =
            arbeidserfaring
                |> Cv.Arbeidserfaring.tildato
                |> Maybe.withDefault ""
        , navarende =
            arbeidserfaring
                |> Cv.Arbeidserfaring.navarende
        , styrkkode =
            arbeidserfaring
                |> Cv.Arbeidserfaring.styrkkode
                |> Maybe.withDefault ""
        , ikkeAktueltForFremtiden = arbeidserfaring |> Cv.Arbeidserfaring.ikkeAktueltForFremtiden
        , yrkeFritekst =
            arbeidserfaring
                |> Cv.Arbeidserfaring.yrkeFritekst
                |> Maybe.withDefault ""
        , konseptid =
            arbeidserfaring
                |> Cv.Arbeidserfaring.konseptid
                |> Maybe.withDefault ""
        , beskrivelse =
            arbeidserfaring
                |> Cv.Arbeidserfaring.beskrivelse
                |> Maybe.withDefault ""
        }


tomtSkjema : ArbeidserfaringSkjema
tomtSkjema =
    ArbeidserfaringSkjema
        { id = ""
        , arbeidsgiver = ""
        , yrke = ""
        , sted = ""
        , fradato = ""
        , tildato = ""
        , navarende = False
        , styrkkode = ""
        , ikkeAktueltForFremtiden = False
        , yrkeFritekst = ""
        , konseptid = ""
        , beskrivelse = ""
        }
