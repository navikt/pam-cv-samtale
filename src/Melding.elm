module Melding exposing
    ( Melding
    , Tekstområde(..)
    , antallOrd
    , eksempel
    , innhold
    , spørsmål
    , spørsmålMedTekstområder
    , svar
    , toHtml
    , tomLinje
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)


type Melding
    = Melding Options


type MeldingsType
    = Spørsmål (List Tekstområde)
    | Eksempel (List Tekstområde)
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


eksempel : List String -> Melding
eksempel list =
    Melding
        { meldingsType =
            list
                |> List.map Avsnitt
                |> Eksempel
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

        Eksempel tekstområder ->
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


antallOrd : Melding -> Int
antallOrd (Melding tekstområder) =
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


toHtml : Melding -> Html msg
toHtml (Melding options) =
    case options.meldingsType of
        Spørsmål _ ->
            article [ class "melding", ariaLive "polite" ]
                (Melding options
                    |> innhold
                    |> List.map viewTekstområde
                )

        Svar _ ->
            article [ class "melding" ]
                (Melding options
                    |> innhold
                    |> List.map viewTekstområde
                )

        Eksempel _ ->
            article [ class "eksempel", ariaLive "polite" ]
                (List.concat
                    [ [ span [ class "eksempel-tittel" ] [ text "Eksempel:" ] ]
                    , Melding options
                        |> innhold
                        |> List.map viewTekstområde
                    ]
                )



{--
                
--}


viewTekstområde : Tekstområde -> Html msg
viewTekstområde tekstområde =
    case tekstområde of
        Avsnitt tekst ->
            viewAvsnitt tekst

        Seksjon labelTekst tekster ->
            section [ ariaLabel labelTekst ]
                (List.map viewAvsnitt tekster)


viewAvsnitt : String -> Html msg
viewAvsnitt string =
    p [] [ text string ]
