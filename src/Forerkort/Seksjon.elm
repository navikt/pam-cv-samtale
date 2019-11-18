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
import Dato exposing (Dato, DatoValidering(..), Måned)
import DebugStatus exposing (DebugStatus)
import Feilmelding
import Forerkort.Skjema as Skjema exposing (FørerkortSkjema, ValidertFørerkortSkjema)
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import FørerkortKode exposing (FørerkortKode)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Attributes.Aria exposing (ariaLive, role)
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
    | VelgNyttFørerkort { valgtFørerkort : Maybe FørerkortKode, feilmelding : Maybe String }
    | RegistrereFraDato { valgtFørerkort : FørerkortKode, dag : String, måned : Maybe Måned, år : String, visFeilmelding : Bool }
    | RegistrereTilDato { valgtFørerkort : FørerkortKode, fraDato : Maybe Dato, dag : String, måned : Maybe Måned, år : String, visFeilmelding : Bool }
    | Oppsummering ValidertFørerkortSkjema
    | EndreSkjema { skjema : FørerkortSkjema, visFeilmelding : Bool }
    | OppsummeringEtterEndring ValidertFørerkortSkjema
    | LagrerFørerkort ValidertFørerkortSkjema
    | LeggTilFlereFørerkort
    | VenterPåAnimasjonFørFullføring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



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
    | BrukerEndrerFraDag String
    | BrukerEndrerFraMåned String
    | BrukerEndrerFraÅr String
    | BrukerVilGåVidereMedFraDato
    | BrukerEndrerTilDag String
    | BrukerEndrerTilMåned String
    | BrukerEndrerTilÅr String
    | BrukerVilGåVidereMedTilDato
    | BrukerVilLagreIOppsummeringen
    | BrukerVilEndreOppsummeringen
    | FraDatoLagreknappTrykket
    | SkjemaEndret SkjemaEndring
    | VilLagreEndretSkjema


type SkjemaEndring
    = Førerkort String
    | FraÅr String
    | FraMåned String
    | FraDag String
    | TilÅr String
    | TilMåned String
    | TilDag String


oppdaterSkjema : SkjemaEndring -> FørerkortSkjema -> FørerkortSkjema
oppdaterSkjema endring skjema =
    case endring of
        Førerkort førerkortKode ->
            førerkortKode
                |> FørerkortKode.stringTilMaybeFørerkortKode
                |> Skjema.oppdaterFørerkort skjema

        FraÅr år ->
            Skjema.oppdaterFraÅr skjema år

        FraMåned måned ->
            måned
                |> Dato.stringTilMaybeMåned
                |> Skjema.oppdaterFraMåned skjema

        FraDag dag ->
            Skjema.oppdaterFraDag skjema dag

        TilÅr år ->
            Skjema.oppdaterTilÅr skjema år

        TilMåned måned ->
            måned
                |> Dato.stringTilMaybeMåned
                |> Skjema.oppdaterTilMåned skjema

        TilDag dag ->
            Skjema.oppdaterTilDag skjema dag


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        HarKlasseB ->
            ( nesteSamtaleSteg model (Melding.svar [ "Ja" ]) (LagrerFørerkort Skjema.klasseB)
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
                            if FørerkortKode.spørOmDatoInfo førerkortKode then
                                ( { valgtFørerkort = førerkortKode, dag = "", måned = Nothing, år = "", visFeilmelding = False }
                                    |> RegistrereFraDato
                                    |> nesteSamtaleSteg model (Melding.svar [ FørerkortKode.term førerkortKode ])
                                , lagtTilSpørsmålCmd model.debugStatus
                                )
                                    |> IkkeFerdig

                            else
                                let
                                    skjema =
                                        Skjema.fraFørerkortKode førerkortKode
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

        BrukerEndrerFraDag dag ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    ( { info | dag = dag }
                        |> RegistrereFraDato
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerEndrerFraÅr år ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    ( { info | år = år }
                        |> RegistrereFraDato
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerEndrerFraMåned måned ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    ( { info | måned = Dato.stringTilMaybeMåned måned }
                        |> RegistrereFraDato
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FraDatoLagreknappTrykket ->
            IkkeFerdig ( Model model, Cmd.none )

        BrukerVilGåVidereMedFraDato ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    case Dato.validerDato { dag = info.dag, måned = info.måned, år = info.år } of
                        DatoValiderer dato ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , fraDato = Just dato
                              , dag = ""
                              , måned = Nothing
                              , år = ""
                              , visFeilmelding = False
                              }
                                |> RegistrereTilDato
                                |> nesteSamtaleSteg model (Melding.svar [ Dato.toString dato ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        DatoValidererIkke ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , dag = info.dag
                              , måned = info.måned
                              , år = info.år
                              , visFeilmelding = True
                              }
                                |> RegistrereFraDato
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        DatoIkkeSkrevetInn ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , fraDato = Nothing
                              , dag = ""
                              , måned = Nothing
                              , år = ""
                              , visFeilmelding = False
                              }
                                |> RegistrereTilDato
                                |> nesteSamtaleSteg model (Melding.svar [ "Gå videre" ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerEndrerTilDag dag ->
            case model.aktivSamtale of
                RegistrereTilDato info ->
                    ( { info | dag = dag }
                        |> RegistrereTilDato
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerEndrerTilMåned måned ->
            case model.aktivSamtale of
                RegistrereTilDato info ->
                    ( { info | måned = Dato.stringTilMaybeMåned måned }
                        |> RegistrereTilDato
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerEndrerTilÅr år ->
            case model.aktivSamtale of
                RegistrereTilDato info ->
                    ( { info | år = år }
                        |> RegistrereTilDato
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilGåVidereMedTilDato ->
            case model.aktivSamtale of
                RegistrereTilDato info ->
                    case Dato.validerDato { dag = info.dag, måned = info.måned, år = info.år } of
                        DatoValiderer tilDato ->
                            ( { førerkort = info.valgtFørerkort
                              , fraDato = info.fraDato
                              , tilDato = Just tilDato
                              }
                                |> Skjema.initValidert
                                |> Oppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ Dato.toString tilDato ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        DatoValidererIkke ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , fraDato = info.fraDato
                              , dag = info.dag
                              , måned = info.måned
                              , år = info.år
                              , visFeilmelding = True
                              }
                                |> RegistrereTilDato
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        DatoIkkeSkrevetInn ->
                            ( { førerkort = info.valgtFørerkort
                              , fraDato = info.fraDato
                              , tilDato = Nothing
                              }
                                |> Skjema.initValidert
                                |> Oppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ "Gå videre" ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreIOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                OppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    ( { skjema = Skjema.uvalidertSkjemaFraValidertSkjema skjema, visFeilmelding = False }
                        |> EndreSkjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Endre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                OppsummeringEtterEndring skjema ->
                    ( { skjema = Skjema.uvalidertSkjemaFraValidertSkjema skjema, visFeilmelding = False }
                        |> EndreSkjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Endre" ])
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreEndretSkjema ->
            case model.aktivSamtale of
                EndreSkjema skjema ->
                    case Skjema.valider skjema.skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> OppsummeringEtterEndring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { skjema = skjema.skjema
                              , visFeilmelding = True
                              }
                                |> EndreSkjema
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreSkjema skjema ->
                    ( EndreSkjema { skjema = oppdaterSkjema skjemaEndring skjema.skjema, visFeilmelding = False }
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig


validertSkjemaTilSetninger : ValidertFørerkortSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    [ "Førerkort: " ++ Skjema.førerkortFraValidertSkjema validertSkjema
    , "Fra dato: " ++ Skjema.fraDatoFraValidertSkjema validertSkjema
    , "Utløper dato: " ++ Skjema.tilDatoFraValidertSkjema validertSkjema
    ]


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


updateEtterLagreKnappTrykket : ModelInfo -> ValidertFørerkortSkjema -> Melding -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema svar =
    IkkeFerdig
        ( skjema
            |> LagrerFørerkort
            |> nesteSamtaleSteg model svar
        , Cmd.batch
            [ leggTilFørerkortAPI skjema
            , lagtTilSpørsmålCmd model.debugStatus
            ]
        )


leggTilFørerkortAPI : ValidertFørerkortSkjema -> Cmd Msg
leggTilFørerkortAPI skjema =
    Api.postFørerkort BackendSvarerPåLagreRequest skjema


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


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
samtaleTilMeldingsLogg model førerkortSeksjon =
    case førerkortSeksjon of
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

        RegistrereFraDato _ ->
            [ Melding.spørsmål [ "Når fikk du førerkortet?" ] ]

        RegistrereTilDato _ ->
            [ Melding.spørsmål [ "Har førerkortet en utløpsdato?" ] ]

        Oppsummering validertFørerkortSkjema ->
            [ Melding.spørsmål
                [ "Du har lagt inn dette:"
                , Melding.tomLinje
                , "Førerkort: " ++ Skjema.førerkortFraValidertSkjema validertFørerkortSkjema
                , "Gyldig fra dato: " ++ Skjema.fraDatoFraValidertSkjema validertFørerkortSkjema
                , "Utløpsdato: " ++ Skjema.tilDatoFraValidertSkjema validertFørerkortSkjema
                , Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
            ]

        EndreSkjema førerkortSkjema ->
            []

        OppsummeringEtterEndring validertFørerkortSkjema ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]


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


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


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

                RegistrereFraDato info ->
                    Containers.inputMedGåVidereKnapp BrukerVilGåVidereMedFraDato
                        [ div [ class "ForerkortSeksjon-datolinje" ]
                            [ Input.input { label = "Dag", msg = BrukerEndrerFraDag } info.dag
                                |> Input.toHtml
                            , Select.select
                                "Måned"
                                BrukerEndrerFraMåned
                                [ ( "", "Velg måned" )
                                , ( "Januar", "Januar" )
                                , ( "Februar", "Februar" )
                                , ( "Mars", "Mars" )
                                , ( "April", "April" )
                                , ( "Mai", "Mai" )
                                , ( "Juni", "Juni" )
                                , ( "Juli", "Juli" )
                                , ( "August", "August" )
                                , ( "September", "September" )
                                , ( "Oktober", "Oktober" )
                                , ( "November", "November" )
                                , ( "Desember", "Desember" )
                                ]
                                --|> Select.withSelected (Dato.månedTilString options.måned)
                                |> Select.withClass "DatoInput-måned"
                                |> Select.toHtml
                            , Input.input { label = "År", msg = BrukerEndrerFraÅr } info.år
                                |> Input.toHtml
                            ]
                        , case Dato.feilmeldingForDato { dag = info.dag, måned = info.måned, år = info.år } of
                            Just feilmelding ->
                                if info.visFeilmelding then
                                    div [ role "alert", ariaLive "assertive" ]
                                        [ div [ class "skjemaelement__feilmelding" ]
                                            [ text feilmelding.feilmelding ]
                                        ]

                                else
                                    text ""

                            Nothing ->
                                text ""
                        ]

                RegistrereTilDato info ->
                    Containers.inputMedGåVidereKnapp BrukerVilGåVidereMedTilDato
                        [ div [ class "ForerkortSeksjon-datolinje" ]
                            [ Input.input { label = "Dag", msg = BrukerEndrerTilDag } info.dag
                                |> Input.toHtml
                            , Select.select
                                "Måned"
                                BrukerEndrerTilMåned
                                [ ( "", "Velg måned" )
                                , ( "Januar", "Januar" )
                                , ( "Februar", "Februar" )
                                , ( "Mars", "Mars" )
                                , ( "April", "April" )
                                , ( "Mai", "Mai" )
                                , ( "Juni", "Juni" )
                                , ( "Juli", "Juli" )
                                , ( "August", "August" )
                                , ( "September", "September" )
                                , ( "Oktober", "Oktober" )
                                , ( "November", "November" )
                                , ( "Desember", "Desember" )
                                ]
                                |> Select.withClass "DatoInput-måned"
                                |> Select.toHtml
                            , Input.input { label = "År", msg = BrukerEndrerTilÅr } info.år
                                |> Input.toHtml
                            ]
                        , case Dato.feilmeldingForDato { dag = info.dag, måned = info.måned, år = info.år } of
                            Just feilmelding ->
                                if info.visFeilmelding then
                                    div [ role "alert", ariaLive "assertive" ]
                                        [ div [ class "skjemaelement__feilmelding" ]
                                            [ text feilmelding.feilmelding ]
                                        ]

                                else
                                    text ""

                            Nothing ->
                                text ""
                        ]

                Oppsummering _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                            |> Knapp.toHtml
                        ]

                EndreSkjema skjema ->
                    Containers.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                        [ Select.select "Førerkort"
                            (Førerkort >> SkjemaEndret)
                            (List.map
                                (\el ->
                                    ( FørerkortKode.kode el, FørerkortKode.term el )
                                )
                                model.førerkortKoder
                            )
                            |> Select.withMaybeSelected (Maybe.map FørerkortKode.kode (Skjema.førerkortKodeFraSkjema skjema.skjema))
                            |> Select.toHtml
                        , div [] [ text "Gyldig fra dato" ]
                        , div [ class "ForerkortSeksjon-datolinje" ]
                            [ Input.inputWithPlaceholder { placeholder = "Dag", label = "", msg = FraDag >> SkjemaEndret } (Skjema.fraDagFraSkjema skjema.skjema)
                                |> Input.toHtml
                            , Select.select
                                ""
                                (FraMåned >> SkjemaEndret)
                                [ ( "", "Måned" )
                                , ( "Januar", "Januar" )
                                , ( "Februar", "Februar" )
                                , ( "Mars", "Mars" )
                                , ( "April", "April" )
                                , ( "Mai", "Mai" )
                                , ( "Juni", "Juni" )
                                , ( "Juli", "Juli" )
                                , ( "August", "August" )
                                , ( "September", "September" )
                                , ( "Oktober", "Oktober" )
                                , ( "November", "November" )
                                , ( "Desember", "Desember" )
                                ]
                                |> Select.withMaybeSelected (Maybe.map Dato.månedTilString (Skjema.fraMånedFraSkjema skjema.skjema))
                                |> Select.withClass "DatoInput-måned"
                                |> Select.toHtml
                            , Input.inputWithPlaceholder { placeholder = "År", label = "", msg = FraÅr >> SkjemaEndret } (Skjema.fraÅrFraSkjema skjema.skjema)
                                |> Input.toHtml
                            ]
                        , case Dato.feilmeldingForDato { dag = Skjema.fraDagFraSkjema skjema.skjema, måned = Skjema.fraMånedFraSkjema skjema.skjema, år = Skjema.fraÅrFraSkjema skjema.skjema } of
                            Just feilmelding ->
                                if skjema.visFeilmelding then
                                    div [ role "alert", ariaLive "assertive" ]
                                        [ div [ class "skjemaelement__feilmelding" ]
                                            [ text feilmelding.feilmelding ]
                                        ]

                                else
                                    text ""

                            Nothing ->
                                text ""
                        , div [] [ text "Utløper dato" ]
                        , div [ class "ForerkortSeksjon-datolinje" ]
                            [ Input.inputWithPlaceholder { placeholder = "Dag", label = "", msg = TilDag >> SkjemaEndret } (Skjema.tilDagFraSkjema skjema.skjema)
                                |> Input.toHtml
                            , Select.select
                                ""
                                (TilMåned >> SkjemaEndret)
                                [ ( "", "Måned" )
                                , ( "Januar", "Januar" )
                                , ( "Februar", "Februar" )
                                , ( "Mars", "Mars" )
                                , ( "April", "April" )
                                , ( "Mai", "Mai" )
                                , ( "Juni", "Juni" )
                                , ( "Juli", "Juli" )
                                , ( "August", "August" )
                                , ( "September", "September" )
                                , ( "Oktober", "Oktober" )
                                , ( "November", "November" )
                                , ( "Desember", "Desember" )
                                ]
                                |> Select.withMaybeSelected (Maybe.map Dato.månedTilString (Skjema.tilMånedFraSkjema skjema.skjema))
                                |> Select.withClass "DatoInput-måned"
                                |> Select.toHtml
                            , Input.inputWithPlaceholder { placeholder = "År", label = "", msg = TilÅr >> SkjemaEndret } (Skjema.tilÅrFraSkjema skjema.skjema)
                                |> Input.toHtml
                            ]
                        , case Dato.feilmeldingForDato { dag = Skjema.tilDagFraSkjema skjema.skjema, måned = Skjema.tilMånedFraSkjema skjema.skjema, år = Skjema.tilÅrFraSkjema skjema.skjema } of
                            Just feilmelding ->
                                if skjema.visFeilmelding then
                                    div [ role "alert", ariaLive "assertive" ]
                                        [ div [ class "skjemaelement__feilmelding" ]
                                            [ text feilmelding.feilmelding ]
                                        ]

                                else
                                    text ""

                            Nothing ->
                                text ""
                        ]

                OppsummeringEtterEndring _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                            |> Knapp.toHtml
                        ]

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
