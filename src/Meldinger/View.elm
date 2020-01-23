module Meldinger.View exposing (viewSuccess)

import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInputType(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Meldinger.Konstanter as Konstanter
import Meldinger.Melding as Melding exposing (Melding, MeldingsType(..), Tekstområde(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (MeldingsGruppeViewState(..), MeldingsLogg, SvarGruppeViewState(..))
import Meldinger.SporsmalViewState as SpørsmålViewState exposing (IkonStatus(..), SpørsmålStyle(..), SpørsmålViewState)


viewSuccess : MeldingsLogg msg -> Html msg -> Html msg
viewSuccess meldingslogg brukerInput =
    div [ class "cv-samtale", id "samtale" ]
        [ div [ id "samtale-innhold" ]
            [ div [ class "samtale-header" ]
                [ i [ class "Robotlogo-header" ] []
                , h1 [] [ text "Få hjelp til å lage CV-en" ]
                , p [] [ text "Her starter samtalen din med roboten" ]
                ]
            , div [ class "samtale-wrapper" ]
                [ div [ class "samtale" ]
                    [ meldingslogg
                        |> MeldingsLogg.meldingsgrupper
                        |> viewMeldingsLogg
                    , div [ classList [ ( "brukerInput-padding", True ) ] ]
                        -- TODO: Fjern hardkodingen av True
                        [ brukerInput ]
                    , div [ class "samtale-padding" ] []
                    ]
                ]
            ]
        ]


viewMeldingsLogg : List (MeldingsGruppeViewState msg) -> Html msg
viewMeldingsLogg meldingsLogg =
    div []
        (List.map viewMeldingsgruppe meldingsLogg)


viewMeldingsgruppe : MeldingsGruppeViewState msg -> Html msg
viewMeldingsgruppe meldingsGruppe =
    case meldingsGruppe of
        SpørsmålGruppe spørsmålGruppe ->
            spørsmålGruppe
                |> MeldingsLogg.mapSpørsmålsgruppe viewSpørsmål
                |> div [ class "meldingsgruppe", ariaLabel "Roboten" ]

        SvarGruppe melding ->
            viewSvar melding


viewSpørsmål : SpørsmålViewState -> Html msg
viewSpørsmål spørsmål =
    let
        spørsmålClass =
            case SpørsmålViewState.meldingsType spørsmål of
                Spørsmål ->
                    "melding "

                SpørsmålMedEksempel ->
                    "melding eksempel "

                Svar ->
                    ""
    in
    div [ class "meldingsrad sporsmal" ]
        [ div [ class "robot", robotAttribute spørsmål ]
            [ i [ class "Robotlogo" ] [] ]
        , case SpørsmålViewState.spørsmålStyle spørsmål of
            FørSkriveindikator ->
                div
                    [ class (spørsmålClass ++ "skjult")
                    , ariaLive "off"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ div [ class "skriver-melding" ] [] ]

            Skriveindikator ->
                div
                    [ class (spørsmålClass ++ "skriveindikator")
                    , ariaLive "off"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ viewSkriveStatus ]

            StørrelseKalkuleres ->
                article
                    [ class (spørsmålClass ++ "kalkulerer")
                    , ariaLive "polite"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ div [ class "meldinginnhold-overflow-hidden" ]
                        [ div [ class "meldinginnhold-wrapper", id "test" ]
                            (spørsmål
                                |> SpørsmålViewState.tekst
                                |> List.map viewTekstområde
                            )
                        ]
                    ]

            MeldingAnimeres { height, width } ->
                let
                    padding =
                        16

                    snakkebobleHeight =
                        Konstanter.meldingHøyde height

                    snakkebobleWidth =
                        width + (2 * padding) + 1
                in
                article
                    [ class (spørsmålClass ++ "ferdiganimert")
                    , ariaLive "polite"
                    , style "height" (String.fromInt snakkebobleHeight ++ "px")
                    , style "width" (String.fromInt snakkebobleWidth ++ "px")
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ div [ class "meldinginnhold-overflow-hidden" ]
                        [ div [ class "meldinginnhold-wrapper" ]
                            (spørsmål
                                |> SpørsmålViewState.tekst
                                |> List.map viewTekstområde
                            )
                        ]
                    ]

            MeldingFerdigAnimert ->
                article
                    [ class spørsmålClass
                    , classList [ ( "ikke-siste", ikkeSisteMelding spørsmål ) ]
                    , ariaLive "polite"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    (spørsmål
                        |> SpørsmålViewState.tekst
                        |> List.map viewTekstområde
                    )
        ]


ikkeSisteMelding : SpørsmålViewState -> Bool
ikkeSisteMelding spørsmål =
    case SpørsmålViewState.ikonStatus spørsmål of
        SkjultIkon ->
            True

        MidtstiltIkonForFørsteSpørsmål ->
            True

        MidtstiltIkon ->
            False

        IkonForNesteMelding _ ->
            True


robotAttribute : SpørsmålViewState -> Html.Attribute msg
robotAttribute spørsmål =
    case SpørsmålViewState.ikonStatus spørsmål of
        SkjultIkon ->
            class "skjult-robot-ikon"

        MidtstiltIkonForFørsteSpørsmål ->
            class "forste-melding"

        MidtstiltIkon ->
            classList []

        IkonForNesteMelding height ->
            transformForRobot height


transformForRobot : { height : Int } -> Html.Attribute msg
transformForRobot { height } =
    let
        avstand =
            (toFloat (Konstanter.meldingHøyde height + Konstanter.skriveIndikatorHøyde) / 2) + toFloat Konstanter.meldingMarginTop
    in
    style "transform" ("translateY(" ++ String.fromFloat avstand ++ "px)")


viewSvar : SvarGruppeViewState msg -> Html msg
viewSvar svarGruppeViewState =
    case svarGruppeViewState of
        FerdigAnimertSvar melding ->
            viewSvarMelding melding

        IkkeFerdigAnimertSvar msg brukerInput ->
            -- TODO: Start animasjon ved å vise brukerInput med en klasse med transition-properties,
            -- _så_ mål avstand dit knappen skal legg på .knapp-til-svar og .knapp-ikke-valgt
            div []
                [ brukerInput
                    |> BrukerInput.tilSvarMelding msg
                    |> viewSvarMelding
                , case BrukerInput.viewType brukerInput of
                    KnapperBrukerInput knapperInfo ->
                        BrukerInput.animerKnapper knapperInfo
                            { valgtKnappClass = "knapp-til-svar"
                            , andreKnapperClass = "knapp-ikke-valgt"
                            , msg = msg
                            }

                    Annet ->
                        text "Annet"
                ]


viewSvarMelding : Melding -> Html msg
viewSvarMelding melding =
    div [ class "meldingsgruppe", ariaLabel "Deg" ]
        [ div [ class "meldingsrad svar" ]
            [ article
                [ class "melding"
                ]
                (melding
                    |> Melding.innhold
                    |> List.map viewTekstområde
                )
            ]
        ]


viewTekstområde : Tekstområde -> Html msg
viewTekstområde tekstområde =
    case tekstområde of
        Avsnitt tekst ->
            viewAvsnitt tekst

        Seksjon labelTekst tekster ->
            section [ ariaLabel labelTekst ]
                (List.map viewAvsnitt tekster)

        Overskrift tekst ->
            span [ class "eksempel-tittel" ] [ text tekst ]


viewAvsnitt : String -> Html msg
viewAvsnitt string =
    p [] [ text string ]


viewSkriveStatus : Html msg
viewSkriveStatus =
    div [ class "skriver-melding" ]
        [ div [ class "bounce bounce1" ] []
        , div [ class "bounce bounce2" ] []
        , div [ class "bounce bounce3" ] []
        ]



--viewBrukerInput : SamtaleSeksjon -> Html Msg
--viewBrukerInput aktivSeksjon =
--    div [ classList [ ( "brukerInput-padding", True ) ] ]
--        -- TODO: Fjern hardkodingen av True
--        [ viewBrukerInputForSeksjon aktivSeksjon
--        ]
