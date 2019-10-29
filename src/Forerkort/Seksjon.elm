module Forerkort.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , update
    , viewBrukerInput
    )

import Api
import Cv.Forerkort as Forerkort exposing (Forerkort, Klasse(..))
import DebugStatus exposing (DebugStatus)
import Feilmelding
import Forerkort.Skjema as Skjema exposing (FørerkortSkjema)
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import FørerkortKode exposing (FørerkortKode)
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import List.Extra as List
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Process
import SamtaleAnimasjon
import String.Extra as String
import Task



-- MODEL --


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , førerkort : List Forerkort
    , førerkortKoder : List FørerkortKode
    , debugStatus : DebugStatus
    }


type Samtale
    = IntroLeggTilKlasseB (List Forerkort)
    | VenterPåAnimasjonFørFullføring
    | LagrerKlasseB FørerkortSkjema
    | LeggTilFlereFørerkort
    | VelgNyttFørerkort { valgtFørerkort : Maybe FørerkortKode, feilmelding : Maybe String }
    | LagrerFørerkort FørerkortSkjema


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


førerkortListe : List FørerkortKode
førerkortListe =
    []



--- UPDATE ---


type Msg
    = HarKlasseB
    | HarIkkeKlasseB
    | BrukerHarFlereFørerkort
    | BrukerVilAvslutteSeksjonen
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt
    | ErrorLogget
    | BackendSvarerPåLagreRequest (Result Http.Error (List Forerkort))
    | BrukerVilGåVidereMedValgtFørerkort
    | BrukerHarValgtFørerkortFraDropdown String


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        HarKlasseB ->
            ( nesteSamtaleSteg model (Melding.svar [ "Ja" ]) (LagrerKlasseB Skjema.klasseB)
            , Cmd.batch
                [ leggTilFørerkortAPI Skjema.klasseB
                , lagtTilSpørsmålCmd model.debugStatus
                ]
            )
                |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ]) VenterPåAnimasjonFørFullføring
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn (always ViewportSatt)
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

        ViewportSatt ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        ErrorLogget ->
            ( Model model, Cmd.none ) |> IkkeFerdig

        HarIkkeKlasseB ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        BrukerHarFlereFørerkort ->
            ( { valgtFørerkort = Nothing, feilmelding = Nothing }
                |> VelgNyttFørerkort
                |> nesteSamtaleSteg model (Melding.svar [ "Ja, legg til førerkort" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BackendSvarerPåLagreRequest result ->
            case result of
                Ok førerkort ->
                    case model.aktivSamtale of
                        LagrerKlasseB _ ->
                            ( nesteSamtaleStegUtenMelding { model | førerkort = førerkort } LeggTilFlereFørerkort
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        LagrerFørerkort _ ->
                            ( nesteSamtaleStegUtenMelding { model | førerkort = førerkort } LeggTilFlereFørerkort
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Err error ->
                    case model.aktivSamtale of
                        _ ->
                            IkkeFerdig ( Model model, logFeilmelding error "Lagre førerkort" )

        BrukerVilGåVidereMedValgtFørerkort ->
            case model.aktivSamtale of
                VelgNyttFørerkort velgNyttFørerkortInfo ->
                    case velgNyttFørerkortInfo.valgtFørerkort of
                        Just førerkortKode ->
                            let
                                skjema =
                                    Skjema.init
                                        { førerkort = førerkortKode
                                        }
                            in
                            ( LagrerFørerkort skjema
                                |> nesteSamtaleSteg model (Melding.svar [ FørerkortKode.term førerkortKode ])
                            , Cmd.batch
                                [ leggTilFørerkortAPI skjema
                                , lagtTilSpørsmålCmd model.debugStatus
                                ]
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( Model
                                { model
                                    | aktivSamtale =
                                        VelgNyttFørerkort
                                            { velgNyttFørerkortInfo
                                                | feilmelding = Just "Velg et førerkort"
                                            }
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BrukerHarValgtFørerkortFraDropdown valgtFørerkort ->
            IkkeFerdig
                ( Model { model | aktivSamtale = updateEtterAtBrukerHarValgtFørerkortFraDropdown model.aktivSamtale model.førerkortKoder valgtFørerkort }
                , Cmd.none
                )


updateEtterAtBrukerHarValgtFørerkortFraDropdown : Samtale -> List FørerkortKode -> String -> Samtale
updateEtterAtBrukerHarValgtFørerkortFraDropdown aktivSamtale remoteFørerkortKoder valgtFørerkort =
    case ( aktivSamtale, remoteFørerkortKoder ) of
        ( VelgNyttFørerkort velgNyttFørerkortInfo, førerkortKoder ) ->
            case List.find (\forerkortKode -> valgtFørerkort == FørerkortKode.kode forerkortKode) førerkortKoder of
                Just førerkortKode ->
                    VelgNyttFørerkort
                        { valgtFørerkort = Just førerkortKode
                        , feilmelding = Nothing
                        }

                Nothing ->
                    VelgNyttFørerkort
                        { velgNyttFørerkortInfo
                            | valgtFørerkort = Nothing
                        }

        _ ->
            aktivSamtale


leggTilFørerkortAPI : FørerkortSkjema -> Cmd Msg
leggTilFørerkortAPI skjema =
    Api.postFørerkort BackendSvarerPåLagreRequest skjema


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


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
                    , SamtaleAnimasjon.scrollTilBunn (always ViewportSatt)
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
        [ SamtaleAnimasjon.scrollTilBunn (always ViewportSatt)
        , 200
            |> DebugStatus.meldingsTimeout debugStatus
            |> Process.sleep
            |> Task.perform (always StartÅSkrive)
        ]


samtaleTilMeldingsLogg : ModelInfo -> Samtale -> List Melding
samtaleTilMeldingsLogg model språkSeksjon =
    case språkSeksjon of
        IntroLeggTilKlasseB førerkortListe_ ->
            if List.isEmpty førerkortListe_ then
                [ Melding.spørsmål [ "Har du førerkort klasse B? Det vil si vanlig førerkort for å kjøre personbil." ]
                ]

            else
                [ Melding.spørsmål [ "Nå skal du legge inn førerkort." ]
                , Melding.spørsmål
                    [ "Jeg ser at du har lagt inn disse førerkortene allerede:"
                    , førerkortListe_
                        |> List.map Forerkort.klasse
                        |> List.map klasseToString
                        |> List.map String.toLower
                        |> listeTilSetning
                        |> String.toSentenceCase
                        |> (\setning -> setning ++ ".")
                    ]
                , Melding.spørsmål [ "Vil du legge til flere?" ]
                ]

        LagrerKlasseB _ ->
            []

        VenterPåAnimasjonFørFullføring ->
            []

        LeggTilFlereFørerkort ->
            [ Melding.spørsmål
                [ "Supert! Da har du lagt inn "
                    ++ (model.førerkort
                            |> List.map Forerkort.klasse
                            |> List.map klasseToString
                            |> List.map String.toLower
                            |> listeTilSetning
                       )
                    ++ "."
                ]
            , Melding.spørsmål
                [ "Har du flere førerkort?"
                ]
            ]

        VelgNyttFørerkort _ ->
            [ Melding.spørsmål [ "Hvilket førerkort vil du legge til?" ] ]

        LagrerFørerkort _ ->
            []


klasseToString : Klasse -> String
klasseToString klasse =
    case klasse of
        Personbil ->
            "Personbil"

        Lastebil ->
            "Lastebil"

        LettLastebil ->
            "Lett lastebil"

        LettLastebilMedTilhenger ->
            "Lett lastebil med tilhenger"

        LastebilMedTilhenger ->
            "Lastebil med tilhenger"

        Minibuss ->
            "Minibuss"

        MinibussMedTilhenger ->
            "Minibuss med tilhenger"

        Buss ->
            "Buss"

        BussMedTilhenger ->
            "Buss med tilhenger"

        Moped ->
            "Moped"

        LettMotorsykkel ->
            "Lett motorsykkel"

        MellomtungMotorsykkel ->
            "Mellomtung motorsykkel"

        TungMotorsykkel ->
            "Tung motorsykkel"

        PersonbilMedTilhenger ->
            "Personbil med tilhenger"

        Traktor ->
            "Traktor"

        Snøscooter ->
            "Snøscooter"


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtaleSeksjon)
        }


nesteSamtaleStegUtenMelding : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenMelding model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtaleSeksjon)
        }


listeTilSetning : List String -> String
listeTilSetning list =
    case List.reverse list of
        [] ->
            ""

        siste :: [] ->
            siste

        siste :: resten ->
            (List.reverse resten |> String.join ", ") ++ " og " ++ siste



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                IntroLeggTilKlasseB førerkortliste ->
                    if List.isEmpty førerkortliste then
                        Containers.knapper Flytende
                            [ Knapp.knapp HarKlasseB "Ja"
                                |> Knapp.toHtml
                            , Knapp.knapp HarIkkeKlasseB "Nei"
                                |> Knapp.toHtml
                            ]

                    else
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                |> Knapp.toHtml
                            ]

                LagrerKlasseB _ ->
                    text ""

                VenterPåAnimasjonFørFullføring ->
                    text ""

                LeggTilFlereFørerkort ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                            |> Knapp.toHtml
                        ]

                VelgNyttFørerkort velgNyttFørerkortInfo ->
                    Containers.inputMedGåVidereKnapp BrukerVilGåVidereMedValgtFørerkort
                        [ div [ class "select-i-samtaleflyt-wrapper" ]
                            [ Select.select "Førerkort"
                                BrukerHarValgtFørerkortFraDropdown
                                (( "Velg førerkort", "Velg førerkort" )
                                    :: List.map
                                        (\el ->
                                            ( FørerkortKode.kode el, FørerkortKode.term el )
                                        )
                                        model.førerkortKoder
                                )
                                |> Select.withMaybeSelected (Maybe.map FørerkortKode.kode velgNyttFørerkortInfo.valgtFørerkort)
                                |> Select.withMaybeFeilmelding velgNyttFørerkortInfo.feilmelding
                                |> Select.toHtml
                            ]
                        ]

                LagrerFørerkort førerkortSkjema ->
                    text ""

        MeldingerGjenstår ->
            text ""



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Forerkort -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg førerkort =
    let
        aktivSamtale =
            IntroLeggTilKlasseB førerkort

        modelInfo =
            { seksjonsMeldingsLogg = MeldingsLogg.tilMeldingsLogg gammelMeldingsLogg
            , aktivSamtale = aktivSamtale
            , førerkort = førerkort
            , førerkortKoder = FørerkortKode.liste
            , debugStatus = debugStatus
            }
    in
    ( Model
        { modelInfo
            | seksjonsMeldingsLogg =
                MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg modelInfo aktivSamtale) modelInfo.seksjonsMeldingsLogg
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd debugStatus
        ]
    )
