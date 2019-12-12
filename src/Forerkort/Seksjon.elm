module Forerkort.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , subscriptions
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Cv.Forerkort as Forerkort exposing (Førerkort, Klasse(..))
import Dato exposing (Dato, DatoValidering(..), Måned)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Forerkort.ForerkortKode as FørerkortKode exposing (FørerkortKode)
import Forerkort.Skjema as Skjema exposing (FørerkortSkjema, ValidertFørerkortSkjema)
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.DatoInputMedDag as DatoInputMedDag
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import LagreStatus exposing (LagreStatus)
import List.Extra as List
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import String.Extra as String
import Task



-- MODEL --


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , førerkort : List Førerkort
    , førerkortKoder : List FørerkortKode
    , debugStatus : DebugStatus
    }


type Samtale
    = IntroLeggTilKlasseB (List Førerkort)
    | SvarteNeiPåKlasseB
    | VelgNyttFørerkort { valgtFørerkort : Maybe FørerkortKode, feilmelding : Maybe String }
    | RegistrereFraDato { valgtFørerkort : FørerkortKode, dag : String, måned : Maybe Måned, år : String, visFeilmelding : Bool }
    | RegistrereTilDato { valgtFørerkort : FørerkortKode, fraDato : Maybe Dato, dag : String, måned : Maybe Måned, år : String, visFeilmelding : Bool }
    | Oppsummering ValidertFørerkortSkjema
    | EndreSkjema { skjema : FørerkortSkjema, visFeilmelding : Bool }
    | OppsummeringEtterEndring ValidertFørerkortSkjema
    | LagrerFørerkort ValidertFørerkortSkjema LagreStatus
    | LagrerFørerkortKlasseB ValidertFørerkortSkjema LagreStatus
    | LagringFeilet Http.Error ValidertFørerkortSkjema
    | LeggTilFlereFørerkort
    | VenterPåAnimasjonFørFullføring (List Førerkort)


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Førerkort) FerdigAnimertMeldingsLogg


type InputId
    = FraDatoId
    | TilDatoId


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = HarKlasseB
    | HarIkkeKlasseB
    | BrukerHarFlereFørerkort
    | BrukerVilAvslutteSeksjonen
    | ErrorLogget
    | FørerkortLagret (Result Http.Error (List Førerkort))
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
    | FerdigMedFørerkort
    | SendSkjemaPåNytt
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())


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
            ( LagreStatus.init
                |> LagrerFørerkortKlasseB Skjema.klasseB
                |> oppdaterSamtale model (SvarFraMsg msg)
            , Cmd.batch
                [ leggTilFørerkortAPI Skjema.klasseB
                , lagtTilSpørsmålCmd model.debugStatus
                ]
            )
                |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            ( VenterPåAnimasjonFørFullføring model.førerkort
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )

        HarIkkeKlasseB ->
            ( SvarteNeiPåKlasseB
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        BrukerHarFlereFørerkort ->
            ( { valgtFørerkort = Nothing, feilmelding = Nothing }
                |> VelgNyttFørerkort
                |> oppdaterSamtale model (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig

        FørerkortLagret result ->
            case model.aktivSamtale of
                LagrerFørerkort skjema lagreStatus ->
                    case result of
                        Ok førerkort ->
                            let
                                oppdatertMeldingslogg =
                                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                            |> MeldingsLogg.leggTilSpørsmål
                                                [ Melding.spørsmål [ "Supert. Nå har du lagt til førerkortet " ++ String.toLower (Skjema.førerkortFraValidertSkjema skjema) ++ "." ]
                                                , Melding.spørsmål [ "Har du andre førerkort? " ]
                                                ]

                                    else
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål
                                                [ Melding.spørsmål [ "Supert. Nå har du lagt til førerkortet " ++ String.toLower (Skjema.førerkortFraValidertSkjema skjema) ++ "." ]
                                                , Melding.spørsmål [ "Har du andre førerkort? " ]
                                                ]
                            in
                            ( LeggTilFlereFørerkort
                                |> oppdaterSamtale { model | førerkort = førerkort, seksjonsMeldingsLogg = oppdatertMeldingslogg } IngenNyeMeldinger
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerFørerkort skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.postFørerkort FørerkortLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre førerkort" error
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> oppdaterSamtale model UtenSvar
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre førerkort" error
                                    ]
                                )
                                    |> IkkeFerdig

                LagrerFørerkortKlasseB skjema lagreStatus ->
                    case result of
                        Ok førerkort ->
                            let
                                oppdatertMeldingslogg =
                                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                            |> MeldingsLogg.leggTilSpørsmål
                                                [ Melding.spørsmål [ "Så bra. Det kan være nyttig informasjon for en arbeidsgiver. " ]
                                                , Melding.spørsmål [ "Har du andre førerkort? " ]
                                                ]

                                    else
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål
                                                [ Melding.spørsmål [ "Så bra. Det kan være nyttig informasjon for en arbeidsgiver. " ]
                                                , Melding.spørsmål [ "Har du andre førerkort? " ]
                                                ]
                            in
                            ( LeggTilFlereFørerkort
                                |> oppdaterSamtale { model | førerkort = førerkort, seksjonsMeldingsLogg = oppdatertMeldingslogg } IngenNyeMeldinger
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerFørerkortKlasseB skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.postFørerkort FørerkortLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre førerkort" error
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> oppdaterSamtale model UtenSvar
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre førerkort" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilGåVidereMedValgtFørerkort ->
            case model.aktivSamtale of
                VelgNyttFørerkort velgNyttFørerkortInfo ->
                    case velgNyttFørerkortInfo.valgtFørerkort of
                        Just førerkortKode ->
                            if FørerkortKode.spørOmDatoInfo førerkortKode then
                                ( { valgtFørerkort = førerkortKode
                                  , dag = ""
                                  , måned = Nothing
                                  , år = ""
                                  , visFeilmelding = False
                                  }
                                    |> RegistrereFraDato
                                    |> oppdaterSamtale model (ManueltSvar (Melding.svar [ FørerkortKode.term førerkortKode ]))
                                , lagtTilSpørsmålCmd model.debugStatus
                                )
                                    |> IkkeFerdig

                            else
                                let
                                    skjema =
                                        Skjema.fraFørerkortKode førerkortKode
                                in
                                ( LagrerFørerkort skjema LagreStatus.init
                                    |> oppdaterSamtale model (ManueltSvar (Melding.svar [ FørerkortKode.term førerkortKode ]))
                                , Cmd.batch
                                    [ leggTilFørerkortAPI skjema
                                    , lagtTilSpørsmålCmd model.debugStatus
                                    ]
                                )
                                    |> IkkeFerdig

                        Nothing ->
                            ( { velgNyttFørerkortInfo
                                | feilmelding = Just "Velg et førerkort"
                              }
                                |> VelgNyttFørerkort
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerHarValgtFørerkortFraDropdown valgtFørerkort ->
            IkkeFerdig
                ( updateEtterAtBrukerHarValgtFørerkortFraDropdown model.aktivSamtale model.førerkortKoder valgtFørerkort
                    |> oppdaterSamtale model IngenNyeMeldinger
                , Cmd.none
                )

        BrukerEndrerFraDag dag ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    ( { info | dag = dag }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ Dato.toString dato ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        DatoValideringsfeil ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , dag = info.dag
                              , måned = info.måned
                              , år = info.år
                              , visFeilmelding = True
                              }
                                |> RegistrereFraDato
                                |> oppdaterSamtale model IngenNyeMeldinger
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
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ "Gå videre" ]))
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                        |> oppdaterSamtale model IngenNyeMeldinger
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
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ Dato.toString tilDato ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        DatoValideringsfeil ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , fraDato = info.fraDato
                              , dag = info.dag
                              , måned = info.måned
                              , år = info.år
                              , visFeilmelding = True
                              }
                                |> RegistrereTilDato
                                |> oppdaterSamtale model IngenNyeMeldinger
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
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ "Gå videre" ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreIOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                OppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering skjema ->
                    ( { skjema = Skjema.uvalidertSkjemaFraValidertSkjema skjema
                      , visFeilmelding = False
                      }
                        |> EndreSkjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                OppsummeringEtterEndring skjema ->
                    ( { skjema = Skjema.uvalidertSkjemaFraValidertSkjema skjema
                      , visFeilmelding = False
                      }
                        |> EndreSkjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
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
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { skjema = skjema.skjema
                              , visFeilmelding = True
                              }
                                |> EndreSkjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreSkjema skjema ->
                    ( { skjema = oppdaterSkjema skjemaEndring skjema.skjema
                      , visFeilmelding = False
                      }
                        |> EndreSkjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        FerdigMedFørerkort ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( model.førerkort
                        |> VenterPåAnimasjonFørFullføring
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SendSkjemaPåNytt ->
            case model.aktivSamtale of
                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerFørerkort skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Api.postFørerkort FørerkortLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagrerFørerkort skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerFørerkort skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagrerFørerkortKlasseB skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerFørerkortKlasseB skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerFørerkort skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.postFørerkort FørerkortLagret skjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        FokusSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )


validertSkjemaTilSetninger : ValidertFørerkortSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    [ "Førerkort: " ++ Skjema.førerkortFraValidertSkjema validertSkjema
    , "Førerrett til: " ++ Skjema.tilDatoFraValidertSkjema validertSkjema
    , "Førerrett fra: " ++ Skjema.fraDatoFraValidertSkjema validertSkjema
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


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertFørerkortSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    IkkeFerdig
        ( LagreStatus.init
            |> LagrerFørerkort skjema
            |> oppdaterSamtale model (SvarFraMsg msg)
        , Cmd.batch
            [ leggTilFørerkortAPI skjema
            , lagtTilSpørsmålCmd model.debugStatus
            ]
        )


leggTilFørerkortAPI : ValidertFørerkortSkjema -> Cmd Msg
leggTilFørerkortAPI skjema =
    Api.postFørerkort FørerkortLagret skjema


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring førerkortListe ->
                    Ferdig førerkortListe ferdigAnimertSamtale

                _ ->
                    ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.batch
                        [ Cmd.map SamtaleAnimasjonMsg cmd
                        , settFokus model.aktivSamtale
                        ]
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
            , Cmd.map SamtaleAnimasjonMsg cmd
            )
                |> IkkeFerdig


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrereFraDato _ ->
            settFokusCmd FraDatoId

        RegistrereTilDato _ ->
            settFokusCmd TilDatoId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        FraDatoId ->
            "førerkort-fradato-id"

        TilDatoId ->
            "førerkort-tildato-id"


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


samtaleTilMeldingsLogg : ModelInfo -> Samtale -> List Melding
samtaleTilMeldingsLogg model førerkortSeksjon =
    case førerkortSeksjon of
        IntroLeggTilKlasseB førerkortListe_ ->
            if List.isEmpty førerkortListe_ then
                [ Melding.spørsmål [ "Da var vi ferdige med språk. Det neste er førerkort." ]
                , Melding.spørsmål [ "Har du førerkort klasse B? Det vil si vanlig førerkort for å kjøre personbil." ]
                ]

            else
                [ Melding.spørsmål [ "Da var vi ferdige med språk. Det neste er førerkort." ]
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
                , Melding.spørsmål [ "Har du andre førerkort?" ]
                ]

        VenterPåAnimasjonFørFullføring _ ->
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
                [ "Har du andre førerkort?"
                ]
            ]

        VelgNyttFørerkort _ ->
            [ Melding.spørsmål [ "Hvilket førerkort har du?" ] ]

        LagrerFørerkort _ _ ->
            []

        LagrerFørerkortKlasseB _ _ ->
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
                , "Førerrett til: " ++ Skjema.tilDatoFraValidertSkjema validertFørerkortSkjema
                , "Førerrett fra " ++ Skjema.fraDatoFraValidertSkjema validertFørerkortSkjema
                , Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
            ]

        EndreSkjema _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        OppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre førerkort" } ]

        SvarteNeiPåKlasseB ->
            [ Melding.spørsmål [ "Ok. Har du andre førerkort?" ] ]


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


svarFraBrukerInput : ModelInfo -> Msg -> Melding
svarFraBrukerInput modelInfo msg =
    modelInfo
        |> modelTilBrukerInput
        |> BrukerInput.tilSvarMelding msg


oppdaterSamtale : ModelInfo -> SamtaleOppdatering Msg -> Samtale -> Model
oppdaterSamtale model meldingsoppdatering samtale =
    Model
        { model
            | aktivSamtale = samtale
            , seksjonsMeldingsLogg =
                case meldingsoppdatering of
                    IngenNyeMeldinger ->
                        model.seksjonsMeldingsLogg

                    SvarFraMsg msg ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtale)

                    ManueltSvar melding ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar melding
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtale)

                    UtenSvar ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model samtale)
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
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            IntroLeggTilKlasseB førerkortliste ->
                if List.isEmpty førerkortliste then
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp HarKlasseB "Ja, jeg har førerkort klasse B"
                        , Knapp.knapp HarIkkeKlasseB "Nei, det har jeg ikke"
                        ]

                else
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                        , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                        ]

            VenterPåAnimasjonFørFullføring _ ->
                BrukerInput.utenInnhold

            LeggTilFlereFørerkort ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                    ]

            VelgNyttFørerkort velgNyttFørerkortInfo ->
                BrukerInput.selectMedGåVidereKnapp BrukerVilGåVidereMedValgtFørerkort
                    (Select.select "Førerkort"
                        BrukerHarValgtFørerkortFraDropdown
                        (( "Velg førerkort", "Velg førerkort" )
                            :: List.map
                                (\el ->
                                    ( FørerkortKode.kode el, FørerkortKode.term el )
                                )
                                model.førerkortKoder
                        )
                        |> Select.withMaybeSelected (Maybe.map FørerkortKode.kode velgNyttFørerkortInfo.valgtFørerkort)
                        |> Select.withFeilmelding velgNyttFørerkortInfo.feilmelding
                        |> Select.withErObligatorisk
                    )

            LagrerFørerkort _ _ ->
                BrukerInput.utenInnhold

            LagrerFørerkortKlasseB _ _ ->
                BrukerInput.utenInnhold

            RegistrereFraDato info ->
                BrukerInput.datoInputMedGåVidereKnapp BrukerVilGåVidereMedFraDato
                    ({ label = "Gyldig fra dato"
                     , onDagChange = BrukerEndrerFraDag
                     , dag = info.dag
                     , år = info.år
                     , onÅrChange = BrukerEndrerFraÅr
                     , måned = info.måned
                     , onMånedChange = BrukerEndrerFraMåned
                     }
                        |> DatoInputMedDag.datoInputMedDag
                        |> DatoInputMedDag.withId (inputIdTilString FraDatoId)
                        |> DatoInputMedDag.withFeilmelding
                            (if info.visFeilmelding then
                                Dato.feilmeldingForDato { dag = info.dag, måned = info.måned, år = info.år }

                             else
                                Nothing
                            )
                    )

            RegistrereTilDato info ->
                BrukerInput.datoInputMedGåVidereKnapp BrukerVilGåVidereMedTilDato
                    ({ label = "Utløper dato"
                     , onDagChange = BrukerEndrerTilDag
                     , dag = info.dag
                     , år = info.år
                     , onÅrChange = BrukerEndrerTilÅr
                     , måned = info.måned
                     , onMånedChange = BrukerEndrerTilMåned
                     }
                        |> DatoInputMedDag.datoInputMedDag
                        |> DatoInputMedDag.withId (inputIdTilString TilDatoId)
                        |> DatoInputMedDag.withFeilmelding
                            (if info.visFeilmelding then
                                Dato.feilmeldingForDato { dag = info.dag, måned = info.måned, år = info.år }

                             else
                                Nothing
                            )
                    )

            Oppsummering _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                    , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                    ]

            EndreSkjema skjema ->
                BrukerInput.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                    [ Select.select "Førerkort"
                        (Førerkort >> SkjemaEndret)
                        (List.map
                            (\el ->
                                ( FørerkortKode.kode el, FørerkortKode.term el )
                            )
                            model.førerkortKoder
                        )
                        |> Select.withMaybeSelected (Maybe.map FørerkortKode.kode (Skjema.førerkortKodeFraSkjema skjema.skjema))
                        |> Select.withErObligatorisk
                        |> Select.toHtml
                    , div [] [ text "Førerrett til" ]
                    , div [ class "ForerkortSeksjon-dato" ]
                        [ { label = "Gyldig til dato"
                          , onDagChange = TilDag >> SkjemaEndret
                          , dag = Skjema.tilDagFraSkjema skjema.skjema
                          , år = Skjema.tilÅrFraSkjema skjema.skjema
                          , onÅrChange = TilÅr >> SkjemaEndret
                          , måned = Skjema.tilMånedFraSkjema skjema.skjema
                          , onMånedChange = TilMåned >> SkjemaEndret
                          }
                            |> DatoInputMedDag.datoInputMedDag
                            |> DatoInputMedDag.withFeilmelding (Dato.feilmeldingForDato { dag = Skjema.tilDagFraSkjema skjema.skjema, måned = Skjema.tilMånedFraSkjema skjema.skjema, år = Skjema.tilÅrFraSkjema skjema.skjema })
                            |> DatoInputMedDag.toHtml
                        , div [] [ text "Førerrett fra" ]
                        , { label = "Gyldig fra dato"
                          , onDagChange = FraDag >> SkjemaEndret
                          , dag = Skjema.fraDagFraSkjema skjema.skjema
                          , år = Skjema.fraÅrFraSkjema skjema.skjema
                          , onÅrChange = FraÅr >> SkjemaEndret
                          , måned = Skjema.fraMånedFraSkjema skjema.skjema
                          , onMånedChange = FraMåned >> SkjemaEndret
                          }
                            |> DatoInputMedDag.datoInputMedDag
                            |> DatoInputMedDag.withFeilmelding (Dato.feilmeldingForDato { dag = Skjema.fraDagFraSkjema skjema.skjema, måned = Skjema.fraMånedFraSkjema skjema.skjema, år = Skjema.fraÅrFraSkjema skjema.skjema })
                            |> DatoInputMedDag.toHtml
                        ]
                    ]

            OppsummeringEtterEndring _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, informasjonen er riktig"
                    , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
                    ]

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    ErrorHåndtering.GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedFørerkort "Gå videre"
                            ]

                    ErrorHåndtering.PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp SendSkjemaPåNytt "Prøv igjen"
                            , Knapp.knapp FerdigMedFørerkort "Gå videre"
                            ]

                    ErrorHåndtering.LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            SvarteNeiPåKlasseB ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                    ]

    else
        BrukerInput.utenInnhold



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Førerkort -> ( Model, Cmd Msg )
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


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
