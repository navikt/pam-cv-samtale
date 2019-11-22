module Melding exposing
    ( Melding
    , MeldingsType(..)
    , Tekstområde(..)
    , antallOrd
    , eksempel
    , eksempelMedTittel
    , innhold
    , meldingstype
    , spørsmål
    , spørsmålMedTekstområder
    , svar
    , tomLinje
    )


type Melding
    = Melding MeldingsType (List Tekstområde)


type MeldingsType
    = Spørsmål
    | SpørsmålMedEksempel
    | Svar


type Tekstområde
    = Avsnitt String
    | Seksjon String (List String)
    | Overskrift String


eksempelMedTittel : String -> List String -> Melding
eksempelMedTittel tittel list =
    [ Overskrift tittel ]
        ++ List.map Avsnitt list
        |> Melding SpørsmålMedEksempel


eksempel : List String -> Melding
eksempel list =
    [ Overskrift "Eksempel: " ]
        ++ List.map Avsnitt list
        |> Melding SpørsmålMedEksempel


spørsmål : List String -> Melding
spørsmål list =
    list
        |> List.map Avsnitt
        |> Melding Spørsmål


spørsmålMedTekstområder : List Tekstområde -> Melding
spørsmålMedTekstområder tekstområder =
    Melding Spørsmål tekstområder


svar : List String -> Melding
svar list =
    list
        |> List.map Avsnitt
        |> Melding Svar


innhold : Melding -> List Tekstområde
innhold (Melding _ tekstområder) =
    tekstområder
        |> List.map splitInnhold
        |> List.concat


meldingstype : Melding -> MeldingsType
meldingstype (Melding meldingsType _) =
    meldingsType


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

        Overskrift string ->
            [ Overskrift string ]


erstattTommeLinjer : String -> String
erstattTommeLinjer linje =
    if (String.trim >> String.isEmpty) linje then
        tomLinje

    else
        linje


tomLinje : String
tomLinje =
    "\u{00A0}"


antallOrd : Melding -> Int
antallOrd (Melding _ tekstområder) =
    tekstområder
        |> List.map antallOrdITekstområde
        |> List.sum


antallOrdITekstområde : Tekstområde -> Int
antallOrdITekstområde tekstområde =
    case tekstområde of
        Avsnitt string ->
            string
                |> String.split " "
                |> List.length

        Seksjon _ list ->
            list
                |> List.map (String.split " " >> List.length)
                |> List.sum

        Overskrift string ->
            string
                |> String.split " "
                |> List.length
