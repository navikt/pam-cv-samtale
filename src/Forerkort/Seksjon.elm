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
import Dato.Dato as Dato exposing (DatoValidering(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Forerkort.Forerkort as Forerkort exposing (Førerkort, Klasse(..))
import Forerkort.ForerkortKode as FørerkortKode exposing (FørerkortKode)
import Forerkort.Skjema as Skjema exposing (FørerkortSkjema, ValidertFørerkortSkjema)
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.DatoInputEttFelt as DatoInputEttFelt
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
    { seksjonsMeldingsLogg : MeldingsLogg Msg
    , aktivSamtale : Samtale
    , førerkort : List Førerkort
    , førerkortKoder : List FørerkortKode
    , debugStatus : DebugStatus
    }


type AvsluttetGrunn
    = AvbruttPåbegynt
    | SlettetPåbegynt
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = IntroLeggTilKlasseB (List Førerkort)
    | SvarteNeiPåKlasseB
    | VelgNyttFørerkort { valgtFørerkort : Maybe FørerkortKode, feilmelding : Maybe String }
    | RegistrereFraDato { valgtFørerkort : FørerkortKode, dato : String, visFeilmelding : Bool }
    | RegistrereTilDato { valgtFørerkort : FørerkortKode, fraDato : Maybe String, tilDato : String, visFeilmelding : Bool }
    | Oppsummering OppsummeringsType ValidertFørerkortSkjema
    | EndreSkjema FørerkortSkjema
    | BekreftSlettingAvPåbegynt ValidertFørerkortSkjema
    | LagrerFørerkort ValidertFørerkortSkjema LagreStatus
    | LagrerFørerkortKlasseB ValidertFørerkortSkjema LagreStatus
    | LagringFeilet Http.Error ValidertFørerkortSkjema
    | LeggTilFlereFørerkort AvsluttetGrunn
    | BekreftAvbrytingAvRegistreringen Samtale
    | VenterPåAnimasjonFørFullføring (List Førerkort)


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Førerkort) FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg Msg
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
    | BrukerEndrerFraDato String
    | BrukerVilGåVidereMedFraDato
    | BrukerEndrerTilDato String
    | BrukerVilGåVidereMedTilDato
    | BrukerVilLagreIOppsummeringen
    | BrukerVilEndreOppsummeringen
    | FraDatoLagreknappTrykket
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | BrukerVilAvbryteRegistreringen
    | BrukerBekrefterAvbrytingAvRegistrering
    | BrukerVilIkkeAvbryteRegistreringen
    | VilLagreEndretSkjema
    | FerdigMedFørerkort
    | SendSkjemaPåNytt
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | DatoMisterFokus
    | TimeoutEtterAtFeltMistetFokus


type SkjemaEndring
    = Førerkort String
    | FraDato String
    | TilDato String
    | FraDatoBlurred
    | TilDatoBlurred


oppdaterSkjema : SkjemaEndring -> FørerkortSkjema -> FørerkortSkjema
oppdaterSkjema endring skjema =
    case endring of
        Førerkort førerkortKode ->
            førerkortKode
                |> FørerkortKode.stringTilMaybeFørerkortKode
                |> Skjema.oppdaterFørerkort skjema

        FraDato år ->
            Skjema.oppdaterFraDato skjema år

        TilDato år ->
            Skjema.oppdaterTilDato skjema år

        FraDatoBlurred ->
            skjema
                |> Skjema.fraDatoFraSkjema
                |> Dato.justerDatoFormat
                |> Skjema.oppdaterFraDato skjema
                |> Skjema.tillatÅViseFeilmeldingFraDato

        TilDatoBlurred ->
            skjema
                |> Skjema.tilDatoFraSkjema
                |> Dato.justerDatoFormat
                |> Skjema.oppdaterTilDato skjema
                |> Skjema.tillatÅViseFeilmeldingTilDato


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
                            ( LeggTilFlereFørerkort AnnenAvslutning
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
                                    , Api.opprettFørerkort FørerkortLagret skjema
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
                            ( LeggTilFlereFørerkort AnnenAvslutning
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
                                    , Api.opprettFørerkort FørerkortLagret skjema
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
                                  , dato = ""
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

        BrukerEndrerFraDato dato ->
            case model.aktivSamtale of
                RegistrereFraDato info ->
                    ( { info | dato = dato }
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
                    case Dato.validerDato info.dato of
                        GyldigDato dato ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , fraDato = Just dato
                              , tilDato = ""
                              , visFeilmelding = False
                              }
                                |> RegistrereTilDato
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ dato ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        DatoValideringsfeil _ ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , dato = info.dato
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
                              , tilDato = ""
                              , visFeilmelding = False
                              }
                                |> RegistrereTilDato
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ "Gå videre" ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerEndrerTilDato dato ->
            case model.aktivSamtale of
                RegistrereTilDato info ->
                    ( { info | tilDato = dato }
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
                    case Dato.validerDato info.tilDato of
                        GyldigDato tilDato ->
                            ( { førerkort = info.valgtFørerkort
                              , fraDato = info.fraDato
                              , tilDato = Just tilDato
                              }
                                |> Skjema.initValidert
                                |> Oppsummering FørsteGang
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ tilDato ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        DatoValideringsfeil _ ->
                            ( { valgtFørerkort = info.valgtFørerkort
                              , fraDato = info.fraDato
                              , tilDato = info.tilDato
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
                                |> Oppsummering FørsteGang
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar [ "Gå videre" ]))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreIOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering _ skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering _ skjema ->
                    ( skjema
                        |> Skjema.uvalidertSkjemaFraValidertSkjema
                        |> EndreSkjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSlettePåbegynt ->
            case model.aktivSamtale of
                Oppsummering _ skjema ->
                    ( BekreftSlettingAvPåbegynt skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BekrefterSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt _ ->
                    ( LeggTilFlereFørerkort SlettetPåbegynt
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AngrerSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt skjema ->
                    ( Oppsummering AvbrøtSletting skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreEndretSkjema ->
            case model.aktivSamtale of
                EndreSkjema skjema ->
                    case Skjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> Oppsummering EtterEndring
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.tillatÅViseAlleFeilmeldinger
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
                    ( skjema
                        |> oppdaterSkjema skjemaEndring
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
                    ( VenterPåAnimasjonFørFullføring model.førerkort
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
                    , Api.opprettFørerkort FørerkortLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilAvbryteRegistreringen ->
            case model.aktivSamtale of
                VelgNyttFørerkort _ ->
                    avbrytRegistrering model msg

                _ ->
                    ( model.aktivSamtale
                        |> BekreftAvbrytingAvRegistreringen
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BrukerBekrefterAvbrytingAvRegistrering ->
            avbrytRegistrering model msg

        BrukerVilIkkeAvbryteRegistreringen ->
            case model.aktivSamtale of
                BekreftAvbrytingAvRegistreringen samtaleStegFørAvbryting ->
                    ( Model
                        { model
                            | aktivSamtale = samtaleStegFørAvbryting
                            , seksjonsMeldingsLogg =
                                model.seksjonsMeldingsLogg
                                    |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                    |> MeldingsLogg.leggTilSpørsmål
                                        (List.concat
                                            [ [ Melding.spørsmål [ "Ok. Da fortsetter vi der vi slapp." ] ]
                                            , samtaleTilMeldingsLogg model samtaleStegFørAvbryting
                                            ]
                                        )
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
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
                                    , Api.opprettFørerkort FørerkortLagret skjema
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

        DatoMisterFokus ->
            case model.aktivSamtale of
                RegistrereFraDato fraDatoInfo ->
                    ( { fraDatoInfo | dato = Dato.justerDatoFormat fraDatoInfo.dato }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , mistetFokusCmd
                    )
                        |> IkkeFerdig

                RegistrereTilDato tilDatoInfo ->
                    ( { tilDatoInfo | tilDato = Dato.justerDatoFormat tilDatoInfo.tilDato }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , mistetFokusCmd
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, mistetFokusCmd )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                RegistrereFraDato fraDatoInfo ->
                    ( { fraDatoInfo | visFeilmelding = True }
                        |> RegistrereFraDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrereTilDato tilDatoInfo ->
                    ( { tilDatoInfo | visFeilmelding = True }
                        |> RegistrereTilDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )


avbrytRegistrering : ModelInfo -> Msg -> SamtaleStatus
avbrytRegistrering model msg =
    ( AvbruttPåbegynt
        |> LeggTilFlereFørerkort
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


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
    Api.opprettFørerkort FørerkortLagret skjema


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg Msg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
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
        IntroLeggTilKlasseB _ ->
            settFokusCmd LeggTilFørerkortIntroId

        SvarteNeiPåKlasseB ->
            settFokusCmd LeggTilForerkortId

        LeggTilFlereFørerkort _ ->
            settFokusCmd LeggTilForerkortId

        VelgNyttFørerkort _ ->
            settFokusCmd VelgForerkortId

        RegistrereFraDato _ ->
            settFokusCmd FraDatoId

        RegistrereTilDato _ ->
            settFokusCmd TilDatoId

        Oppsummering _ _ ->
            settFokusCmd BekreftOppsummeringId

        EndreSkjema _ ->
            settFokusCmd VelgForerkortId

        BekreftSlettingAvPåbegynt _ ->
            settFokusCmd SlettePåbegyntId

        LagringFeilet _ _ ->
            settFokusCmd LagringFeiletActionId

        BekreftAvbrytingAvRegistreringen _ ->
            settFokusCmd AvbrytSlettingId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


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

        LeggTilFlereFørerkort avsluttetGrunn ->
            case avsluttetGrunn of
                AvbruttPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg avbrutt." ]
                    , Melding.spørsmål [ "Har du andre førerkort?" ]
                    ]

                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg slettet førerkortet. Har du andre førerkort?" ] ]

                AnnenAvslutning ->
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
                    , Melding.spørsmål [ "Har du andre førerkort?" ]
                    ]

        BekreftAvbrytingAvRegistreringen _ ->
            [ Melding.spørsmål [ "Hvis du avbryter, blir ikke dette førerkortet lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

        VelgNyttFørerkort _ ->
            [ Melding.spørsmål [ "Hvilket førerkort har du?" ] ]

        LagrerFørerkort _ _ ->
            []

        LagrerFørerkortKlasseB _ _ ->
            []

        RegistrereFraDato _ ->
            [ Melding.spørsmål [ "Når fikk du dette førerkortet?" ] ]

        RegistrereTilDato _ ->
            [ Melding.spørsmål [ "Når utløper førerkortet?" ] ]

        Oppsummering oppsummeringsType validertSkjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Ok, da lar jeg førerkortet stå." ]
                    , oppsummeringsSpørsmål validertSkjema
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål validertSkjema
                    ]

        EndreSkjema _ ->
            [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

        BekreftSlettingAvPåbegynt skjema ->
            let
                førerkort =
                    String.toLower (Skjema.førerkortFraValidertSkjema skjema)
            in
            [ Melding.spørsmål [ "Er du sikker på at du vil slette førerkortet for " ++ førerkort ++ "?" ] ]

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre førerkort" } ]

        SvarteNeiPåKlasseB ->
            [ Melding.spørsmål [ "Ok. Har du andre førerkort?" ] ]


oppsummeringsSpørsmål : ValidertFørerkortSkjema -> Melding
oppsummeringsSpørsmål skjema =
    Melding.spørsmål
        [ "Du har lagt inn dette:"
        , Melding.tomLinje
        , "Førerkort: " ++ Skjema.førerkortFraValidertSkjema skjema
        , "Førerrett fra: " ++ Skjema.fraDatoFraValidertSkjema skjema
        , "Utløpsdato: " ++ Skjema.tilDatoFraValidertSkjema skjema
        , Melding.tomLinje
        , "Er informasjonen riktig?"
        ]


klasseToString : Klasse -> String
klasseToString klasse =
    case klasse of
        Personbil ->
            "B - Personbil"

        PersonbilMedTilhenger ->
            "BE - Personbil med tilhenger"

        LettLastebil ->
            "C1 - Lett lastebil"

        LettLastebilMedTilhenger ->
            "C1E - Lett lastebil med tilhenger"

        Lastebil ->
            "C - Lastebil"

        LastebilMedTilhenger ->
            "CE - Lastebil med tilhenger"

        Minibuss ->
            "D1 - Minibuss"

        MinibussMedTilhenger ->
            "D1E - Minibuss med tilhenger"

        Buss ->
            "D - Buss"

        BussMedTilhenger ->
            "DE - Buss med tilhenger"

        Moped ->
            "AM - Moped"

        LettMotorsykkel ->
            "A1 - Lett motorsykkel"

        MellomtungMotorsykkel ->
            "A2 - Mellomtung motorsykkel"

        TungMotorsykkel ->
            "A - Tung motorsykkel"

        Traktor ->
            "T - Traktor"

        Snøscooter ->
            "S - Snøscooter"


mistetFokusCmd : Cmd Msg
mistetFokusCmd =
    Process.sleep 100
        |> Task.perform (\_ -> TimeoutEtterAtFeltMistetFokus)


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


type InputId
    = LeggTilFørerkortIntroId
    | LeggTilForerkortId
    | VelgForerkortId
    | FraDatoId
    | TilDatoId
    | BekreftOppsummeringId
    | SlettePåbegyntId
    | LagringFeiletActionId
    | AvbrytSlettingId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        LeggTilFørerkortIntroId ->
            "førerkort-legg-til-intro-id"

        LeggTilForerkortId ->
            "førerkort-legg-til-id"

        VelgForerkortId ->
            "førerkort-velg-id"

        FraDatoId ->
            "førerkort-fradato-id"

        TilDatoId ->
            "førerkort-tildato-id"

        BekreftOppsummeringId ->
            "førerkort-bekreft-oppsummering-id"

        SlettePåbegyntId ->
            "førerkort-slett-påbegynt-id"

        LagringFeiletActionId ->
            "førerkort-lagring-feilet-id"

        AvbrytSlettingId ->
            "førerkort-avbrytt-slett-id"


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
                            |> Knapp.withId (inputIdTilString LeggTilFørerkortIntroId)
                        , Knapp.knapp HarIkkeKlasseB "Nei, det har jeg ikke"
                        ]

                else
                    BrukerInput.knapper Flytende
                        [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                            |> Knapp.withId (inputIdTilString LeggTilFørerkortIntroId)
                        , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                        ]

            VenterPåAnimasjonFørFullføring _ ->
                BrukerInput.utenInnhold

            LeggTilFlereFørerkort _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                        |> Knapp.withId (inputIdTilString LeggTilForerkortId)
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                    ]

            VelgNyttFørerkort velgNyttFørerkortInfo ->
                BrukerInput.selectMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilGåVidereMedValgtFørerkort }
                    (Select.select "Førerkort"
                        BrukerHarValgtFørerkortFraDropdown
                        (( "Velg førerkort", "Velg førerkort" )
                            :: List.map
                                (\el ->
                                    ( FørerkortKode.kode el, FørerkortKode.kode el ++ " - " ++ FørerkortKode.term el )
                                )
                                model.førerkortKoder
                        )
                        |> Select.withMaybeSelected (Maybe.map FørerkortKode.kode velgNyttFørerkortInfo.valgtFørerkort)
                        |> Select.withFeilmelding velgNyttFørerkortInfo.feilmelding
                        |> Select.withErObligatorisk
                        |> Select.withId (inputIdTilString VelgForerkortId)
                    )

            LagrerFørerkort _ _ ->
                BrukerInput.utenInnhold

            LagrerFørerkortKlasseB _ _ ->
                BrukerInput.utenInnhold

            RegistrereFraDato info ->
                BrukerInput.datoInputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilGåVidereMedFraDato }
                    ({ label = "Førerrett fra (dd.mm.åååå)"
                     , dato = info.dato
                     , onDatoChange = BrukerEndrerFraDato
                     }
                        |> DatoInputEttFelt.datoInputEttFelt
                        |> DatoInputEttFelt.withId (inputIdTilString FraDatoId)
                        |> DatoInputEttFelt.withWrapperClass "datoInputEttFelt-samtalewrapper"
                        |> DatoInputEttFelt.withOnBlur DatoMisterFokus
                        |> DatoInputEttFelt.withFeilmelding
                            (if info.visFeilmelding then
                                Dato.feilmeldingForDato info.dato

                             else
                                Nothing
                            )
                    )

            RegistrereTilDato info ->
                BrukerInput.datoInputMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilGåVidereMedTilDato }
                    ({ label = "Utløpsdato (dd.mm.åååå)"
                     , dato = info.tilDato
                     , onDatoChange = BrukerEndrerTilDato
                     }
                        |> DatoInputEttFelt.datoInputEttFelt
                        |> DatoInputEttFelt.withId (inputIdTilString TilDatoId)
                        |> DatoInputEttFelt.withWrapperClass "datoInputEttFelt-samtalewrapper"
                        |> DatoInputEttFelt.withOnBlur DatoMisterFokus
                        |> DatoInputEttFelt.withFeilmelding
                            (if info.visFeilmelding then
                                Dato.feilmeldingForDato info.tilDato

                             else
                                Nothing
                            )
                    )

            Oppsummering _ _ ->
                viewBekreftOppsummering

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
                        |> Select.withMaybeSelected (Maybe.map FørerkortKode.kode (Skjema.førerkortKodeFraSkjema skjema))
                        |> Select.withErObligatorisk
                        |> Select.withId (inputIdTilString VelgForerkortId)
                        |> Select.toHtml
                    , div [ class "forerkortSkjema-datoWrapper" ]
                        [ { label = "Førerrett fra (dd.mm.åååå)"
                          , dato = Skjema.fraDatoFraSkjema skjema
                          , onDatoChange = FraDato >> SkjemaEndret
                          }
                            |> DatoInputEttFelt.datoInputEttFelt
                            |> DatoInputEttFelt.withOnBlur (SkjemaEndret FraDatoBlurred)
                            |> DatoInputEttFelt.withFeilmelding (Skjema.feilmeldingFraDato skjema)
                            |> DatoInputEttFelt.toHtml
                        , { label = "Utløpsdato (dd.mmm.åååå)"
                          , dato = Skjema.tilDatoFraSkjema skjema
                          , onDatoChange = TilDato >> SkjemaEndret
                          }
                            |> DatoInputEttFelt.datoInputEttFelt
                            |> DatoInputEttFelt.withWrapperClass ""
                            |> DatoInputEttFelt.withOnBlur (SkjemaEndret TilDatoBlurred)
                            |> DatoInputEttFelt.withFeilmelding (Skjema.feilmeldingTilDato skjema)
                            |> DatoInputEttFelt.toHtml
                        ]
                    ]

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                        |> Knapp.withId (inputIdTilString SlettePåbegyntId)
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                    ]

            BekreftAvbrytingAvRegistreringen _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerBekrefterAvbrytingAvRegistrering "Ja, jeg vil avbryte"
                        |> Knapp.withId (inputIdTilString AvbrytSlettingId)
                    , Knapp.knapp BrukerVilIkkeAvbryteRegistreringen "Nei, jeg vil fortsette"
                    ]

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    ErrorHåndtering.GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedFørerkort "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    ErrorHåndtering.PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp SendSkjemaPåNytt "Prøv igjen"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp FerdigMedFørerkort "Gå videre"
                            ]

                    ErrorHåndtering.LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            SvarteNeiPåKlasseB ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerHarFlereFørerkort "Ja, legg til førerkort"
                        |> Knapp.withId (inputIdTilString LeggTilForerkortId)
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                    ]

    else
        BrukerInput.utenInnhold


viewBekreftOppsummering : BrukerInput Msg
viewBekreftOppsummering =
    BrukerInput.knapper Kolonne
        [ Knapp.knapp BrukerVilLagreIOppsummeringen "Ja, det er riktig"
            |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
        , Knapp.knapp BrukerVilEndreOppsummeringen "Nei, jeg vil endre"
        , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
        ]



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
