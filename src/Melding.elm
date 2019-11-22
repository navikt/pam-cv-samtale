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
    = Melding Options


type MeldingsType
    = Spørsmål (List Tekstområde)
    | SpørsmålMedEksempel String (List Tekstområde)
    | Svar (List Tekstområde)


type Tekstområde
    = Avsnitt String
    | Seksjon String (List String)


type alias MeldingsOptions =
    { meldingsType : MeldingsType
    }


type alias Options =
    { meldingsType : MeldingsType
    , withAriaLive : Bool
    }


melding : MeldingsOptions -> Melding
melding options =
    Melding
        { meldingsType = options.meldingsType
        , withAriaLive = False
        }


eksempelMedTittel : String -> List String -> Melding
eksempelMedTittel tittel list =
    Melding
        { meldingsType =
            list
                |> List.map Avsnitt
                |> SpørsmålMedEksempel tittel
        , withAriaLive = False
        }


eksempel : List String -> Melding
eksempel list =
    Melding
        { meldingsType =
            list
                |> List.map Avsnitt
                |> SpørsmålMedEksempel "Eksempel: "
        , withAriaLive = False
        }


spørsmål : List String -> Melding
spørsmål list =
    Melding
        { meldingsType =
            list
                |> List.map Avsnitt
                |> Spørsmål
        , withAriaLive = False
        }


spørsmålMedTekstområder : List Tekstområde -> Melding
spørsmålMedTekstområder tekstområder =
    Melding
        { meldingsType = Spørsmål tekstområder
        , withAriaLive = False
        }


svar : List String -> Melding
svar list =
    Melding
        { meldingsType =
            list
                |> List.map Avsnitt
                |> Svar
        , withAriaLive = False
        }


innhold : Melding -> List Tekstområde
innhold (Melding options) =
    case options.meldingsType of
        Spørsmål tekstområder ->
            tekstområder
                |> List.map splitInnhold
                |> List.concat

        Svar tekstområder ->
            tekstområder
                |> List.map splitInnhold
                |> List.concat

        SpørsmålMedEksempel _ tekstområder ->
            tekstområder
                |> List.map splitInnhold
                |> List.concat


meldingstype : Melding -> MeldingsType
meldingstype (Melding options) =
    options.meldingsType


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


antallOrd : Melding -> Int
antallOrd (Melding options) =
    case options.meldingsType of
        Spørsmål tekstområder ->
            tekstområder
                |> List.map antallOrdITekstområde
                |> List.sum

        Svar tekstområder ->
            tekstområder
                |> List.map antallOrdITekstområde
                |> List.sum

        SpørsmålMedEksempel tittel tekstområder ->
            ([ tekstområder
             , [ Avsnitt tittel ]
             ]
                |> List.concat
            )
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
