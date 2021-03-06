module Metrikker exposing (Seksjon(..), seksjonTilString)


type Seksjon
    = Loading
    | Failure
    | Intro
    | Personalia
    | Utdanning
    | Arbeidserfaring
    | Språk
    | Fagdokumentasjon
    | Godkjenning
    | Sertifikat
    | AnnenErfaring
    | Førerkort
    | Kurs
    | LeggTilFagdokumentasjoner
    | LeggTilAnnet
    | Sammendrag
    | Synlighet
    | Jobbprofil
    | Eures
    | Tilbakemelding
    | Slutten


seksjonTilString : Seksjon -> String
seksjonTilString seksjon =
    case seksjon of
        Loading ->
            "loading"

        Failure ->
            "failure"

        Intro ->
            "intro"

        Personalia ->
            "personalia"

        Utdanning ->
            "utdanning"

        Arbeidserfaring ->
            "arbeidserfaring"

        Språk ->
            "språk"

        Fagdokumentasjon ->
            "fagdokumentasjon"

        Godkjenning ->
            "godkjenning"

        Sertifikat ->
            "sertifikat"

        AnnenErfaring ->
            "annen-erfaring"

        Førerkort ->
            "førerkort"

        Kurs ->
            "kurs"

        LeggTilFagdokumentasjoner ->
            "legg-til-fagdokumentasjoner"

        LeggTilAnnet ->
            "legg-til-annet"

        Sammendrag ->
            "sammendrag"

        Synlighet ->
            "synlighet"

        Jobbprofil ->
            "jobbprofil"

        Eures ->
            "eures"

        Tilbakemelding ->
            "tilbakemelding"

        Slutten ->
            "slutten"
