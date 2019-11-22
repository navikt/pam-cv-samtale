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
    = Melding MeldingsType


type MeldingsType
    = Spørsmål (List Tekstområde)
    | SpørsmålMedEksempel (List Tekstområde)
    | Svar (List Tekstområde)


type Tekstområde
    = Avsnitt String
    | Seksjon String (List String)
    | Overskrift String


eksempelMedTittel : String -> List String -> Melding
eksempelMedTittel tittel list =
    [ Overskrift tittel ]
        ++ List.map Avsnitt list
        |> SpørsmålMedEksempel
        |> Melding


eksempel : List String -> Melding
eksempel list =
    [ Overskrift "Eksempel: " ]
        ++ List.map Avsnitt list
        |> SpørsmålMedEksempel
        |> Melding


spørsmål : List String -> Melding
spørsmål list =
    list
        |> List.map Avsnitt
        |> Spørsmål
        |> Melding


spørsmålMedTekstområder : List Tekstområde -> Melding
spørsmålMedTekstområder tekstområder =
    Melding (Spørsmål tekstområder)


svar : List String -> Melding
svar list =
    list
        |> List.map Avsnitt
        |> Svar
        |> Melding


innhold : Melding -> List Tekstområde
innhold (Melding meldingsType) =
    case meldingsType of
        Spørsmål tekstområder ->
            tekstområder
                |> List.map splitInnhold
                |> List.concat

        Svar tekstområder ->
            tekstområder
                |> List.map splitInnhold
                |> List.concat

        SpørsmålMedEksempel tekstområder ->
            tekstområder
                |> List.map splitInnhold
                |> List.concat


meldingstype : Melding -> MeldingsType
meldingstype (Melding meldingsType) =
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
antallOrd (Melding meldingsType) =
    let
        tekstområder =
            case meldingsType of
                Spørsmål tekstområder_ ->
                    tekstområder_

                Svar tekstområder_ ->
                    tekstområder_

                SpørsmålMedEksempel tekstområder_ ->
                    tekstområder_
    in
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
