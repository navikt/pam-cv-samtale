module Seksjon.Seksjonsvalg exposing (Model(..), ModelInfo, Msg(..), Samtale(..), SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

-- MODEL --

import Browser.Dom as Dom
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Task


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    }


type Samtale
    = Intro
    | VenterPåAnimasjonFørFullføring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig String Model FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



-- UPDATE --


type Msg
    = StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | GåTilSeksjon String


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        ViewportSatt result ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , Process.sleep 1000
                    |> Task.perform (\_ -> FullførMelding)
                ]
            )
                |> IkkeFerdig

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        GåTilSeksjon seksjon ->
            fullførSeksjonHvisMeldingsloggErFerdig seksjon
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ seksjon ])
                }


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


fullførSeksjonHvisMeldingsloggErFerdig : String -> ModelInfo -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig seksjon modelInfo =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig seksjon (Model modelInfo) ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring }, Cmd.none )
                |> IkkeFerdig


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring ->
                    Ferdig "" (Model model) ferdigAnimertSamtale

                _ ->
                    ( Model
                        { model
                            | seksjonsMeldingsLogg =
                                nyMeldingsLogg
                        }
                    , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        nyMeldingsLogg
                }
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig


lagtTilSpørsmålCmd : Cmd Msg
lagtTilSpørsmålCmd =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , Process.sleep 200
            |> Task.perform (\_ -> StartÅSkrive)
        ]


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg avslutningsSeksjon =
    case avslutningsSeksjon of
        VenterPåAnimasjonFørFullføring ->
            []

        Intro ->
            [ Melding.spørsmål [ "Nå begynner CV-en din å ta form. Er det ner mer du trenger å legge inn?" ]
            , Melding.spørsmål [ "Vil du legge til noen av disse kategoriene?" ]
            ]



-- VIEW --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring ->
                    text ""

                Intro ->
                    div [ class "skjema" ]
                        [ div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Utdanning") "Utdanning"
                                    |> Knapp.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Arbeidserfaring") "Arbeidserfaring"
                                    |> Knapp.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Språk") "Språk"
                                    |> Knapp.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Fagbrev/Svennebrev") "Fagbrev/Svennebrev"
                                    |> Knapp.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Mesterbrev") "Mesterbrev"
                                    |> Knapp.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Autorisasjon") "Autorisasjon"
                                    |> Knapp.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Sertifisering") "Sertifisering"
                                    |> Knapp.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Knapp.knapp (GåTilSeksjon "Nei, gå videre") "Nei, gå videre"
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

        MeldingerGjenstår ->
            text ""



-- INIT --


init : FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
init gammelMeldingsLogg =
    let
        aktivSamtale =
            Intro
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd
        ]
    )
