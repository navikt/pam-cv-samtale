module Melding exposing
    ( Melding
    , Tekstområde(..)
    , innhold
    , spørsmål
    , spørsmålMedTekstområder
    , svar
    , tomLinje
    )


type Melding
    = Melding (List Tekstområde)


type Tekstområde
    = Avsnitt String
    | Seksjon String (List String)


spørsmål : List String -> Melding
spørsmål list =
    list
        |> List.map Avsnitt
        |> Melding


spørsmålMedTekstområder : List Tekstområde -> Melding
spørsmålMedTekstområder tekstområder =
    Melding tekstområder


svar : List String -> Melding
svar list =
    list
        |> List.map Avsnitt
        |> Melding


innhold : Melding -> List Tekstområde
innhold (Melding tekstområder) =
    tekstområder
        |> List.map splitInnhold
        |> List.concat


splitInnhold : Tekstområde -> List Tekstområde
splitInnhold tekstområde =
    case tekstområde of
        Avsnitt tekst ->
            tekst
                |> String.split "\n"
                |> List.map erstattTommeLinjer
                |> List.map Avsnitt

        Seksjon label tekster ->
            tekster
                |> List.concatMap (String.split "\n")
                |> List.map erstattTommeLinjer
                |> Seksjon label
                |> List.singleton


erstattTommeLinjer : String -> String
erstattTommeLinjer linje =
    if (String.trim >> String.isEmpty) linje then
        tomLinje

    else
        linje


tomLinje : String
tomLinje =
    "\u{00A0}"
