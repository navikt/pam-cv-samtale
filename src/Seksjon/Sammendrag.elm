module Seksjon.Sammendrag exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Cv.Sammendrag exposing (Sammendrag)
import DebugStatus exposing (DebugStatus)
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Task



-- MODEL --


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , sammendrag : String
    , debugStatus : DebugStatus
    }


type Samtale
    = BekreftOriginal String
    | EndreOriginal String
    | LagrerEndring String
    | LagringFeilet Http.Error String
    | VenterPåAnimasjonFørFullføring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



-- UPDATE --


type Msg
    = OriginalSammendragBekreftet
    | BrukerVilEndreSammendrag String
    | SammendragEndret String
    | BrukerVilLagreSammendrag String
    | SammendragOppdatert (Result Http.Error Sammendrag)
    | BrukerVilAvslutteSeksjonen
    | ViewportSatt (Result Dom.Error ())
    | FokusSatt (Result Dom.Error ())
    | StartÅSkrive
    | FullførMelding
    | ErrorLogget


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        OriginalSammendragBekreftet ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ "Nei, gå videre" ])
                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Flott! Da er vi nesten ferdige!" ] ]
                    , aktivSamtale = VenterPåAnimasjonFørFullføring
                }
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilEndreSammendrag sammendrag ->
            ( model.sammendrag
                |> EndreOriginal
                |> nesteSamtaleSteg model
                    (if model.sammendrag == "" then
                        Melding.svar [ "Jeg vil legge til" ]

                     else
                        Melding.svar [ "Ja, jeg vil se over" ]
                    )
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        SammendragEndret nyttSammendrag ->
            ( oppdaterSamtaleSteg { model | sammendrag = nyttSammendrag } (EndreOriginal nyttSammendrag)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerVilLagreSammendrag sammendrag ->
            case model.aktivSamtale of
                LagringFeilet error feiletSammendrag ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Prøv på nytt" ]) (LagrerEndring feiletSammendrag)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , leggSammendragTilAPI sammendrag
                        ]
                    )
                        |> IkkeFerdig

                EndreOriginal _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ sammendrag ]) (LagrerEndring sammendrag)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd model.debugStatus
                        , leggSammendragTilAPI sammendrag
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ]) VenterPåAnimasjonFørFullføring
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

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
                , (MeldingsLogg.nesteMeldingToString model.seksjonsMeldingsLogg * 1000.0)
                    |> DebugStatus.meldingsTimeout model.debugStatus
                    |> Process.sleep
                    |> Task.perform (always FullførMelding)
                ]
            )
                |> IkkeFerdig

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        SammendragOppdatert result ->
            case model.aktivSamtale of
                LagrerEndring sammendrag ->
                    case result of
                        Ok value ->
                            ( Model
                                { model
                                    | sammendrag = Cv.Sammendrag.sammendrag value
                                    , aktivSamtale = VenterPåAnimasjonFørFullføring
                                    , seksjonsMeldingsLogg =
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Veldig bra! Nå er vi ferdig med det vanskeligste 😊" ] ]
                                }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( nesteSamtaleSteg model (Melding.spørsmål [ "Oisann.. Klarte ikke å lagre!" ]) (LagringFeilet error sammendrag)
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , sammendrag
                                    |> Api.encodeSammendrag
                                    |> Api.logErrorWithRequestBody ErrorLogget "Lagre sammendrag" error
                                ]
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )


leggSammendragTilAPI : String -> Cmd Msg
leggSammendragTilAPI sammendrag =
    Api.putSammendrag SammendragOppdatert sammendrag


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring ->
                    Ferdig ferdigAnimertSamtale

                _ ->
                    ( Model
                        { model
                            | seksjonsMeldingsLogg =
                                nyMeldingsLogg
                        }
                    , Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        , case model.aktivSamtale of
                            EndreOriginal _ ->
                                settFokusCmd sammendragId

                            _ ->
                                Cmd.none
                        ]
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        nyMeldingsLogg
                }
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , 200
            |> DebugStatus.meldingsTimeout debugStatus
            |> Process.sleep
            |> Task.perform (always StartÅSkrive)
        ]


settFokusCmd : String -> Cmd Msg
settFokusCmd id =
    id
        |> Dom.focus
        |> Task.attempt FokusSatt


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg sammendragSeksjon =
    case sammendragSeksjon of
        BekreftOriginal sammendrag ->
            if sammendrag == "" then
                [ Melding.spørsmål [ "Nå skal du skrive et sammendrag. Her har du mulighet til å selge deg inn. Fortell arbeidsgivere om kompetansen din og personlige egenskaper." ] ]

            else
                [ Melding.spørsmål [ "Nå skal vi skrive et sammendrag." ]
                , Melding.spørsmål
                    [ "Du har allerede skrevet dette..."
                    ]
                , Melding.spørsmål [ sammendrag ]
                , Melding.spørsmål [ "Vil du legge til eller endre på noe?" ]
                ]

        EndreOriginal string ->
            [ Melding.spørsmål [ "Ok! Fyll ut sammendraget ditt i boksen under." ] ]

        LagrerEndring string ->
            []

        LagringFeilet error string ->
            [ Melding.spørsmål
                [ "Sjekk at du er på internett og prøv igjen!" ]
            ]

        VenterPåAnimasjonFørFullføring ->
            [ Melding.spørsmål [ "Veldig bra! Nå er vi ferdig med det vanskeligste 😊" ] ]


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


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }



-- VIEW --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            case model.aktivSamtale of
                BekreftOriginal sammendrag ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp (BrukerVilEndreSammendrag sammendrag)
                                    (if sammendrag == "" then
                                        "Jeg vil legge til sammendrag"

                                     else
                                        "Ja, jeg vil se over"
                                    )
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                EndreOriginal sammendrag ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ Textarea.textarea { label = "Sammendrag", msg = SammendragEndret } sammendrag
                                |> Textarea.withTextAreaClass "textarea_stor"
                                |> Textarea.withId sammendragId
                                |> Textarea.toHtml
                            , Knapp.knapp (BrukerVilLagreSammendrag sammendrag) "Gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                LagrerEndring string ->
                    text ""

                LagringFeilet error sammendrag ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp (BrukerVilLagreSammendrag sammendrag) "Prøv på nytt"
                                |> Knapp.withClass Knapp.SpråknivåKnapp
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Gå videre uten å lagre"
                                |> Knapp.withClass Knapp.SpråknivåKnapp
                                |> Knapp.toHtml
                            ]
                        ]

                VenterPåAnimasjonFørFullføring ->
                    text ""

        MeldingerGjenstår ->
            text ""


sammendragId : String
sammendragId =
    "sammendrag-input"



-- INIT --


init : DebugStatus -> FerdigAnimertMeldingsLogg -> Maybe Sammendrag -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg sammendrag =
    let
        sammendragToString =
            case sammendrag of
                Just a ->
                    Cv.Sammendrag.sammendrag a

                Nothing ->
                    ""

        aktivSamtale =
            BekreftOriginal sammendragToString
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , sammendrag = sammendragToString
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )
