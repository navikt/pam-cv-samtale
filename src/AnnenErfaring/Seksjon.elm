module AnnenErfaring.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , subscriptions
    , update
    , viewBrukerInput
    )

import AnnenErfaring.Skjema as Skjema exposing (AnnenErfaringSkjema, Felt(..), ValidertAnnenErfaringSkjema)
import Api
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Cv.AnnenErfaring exposing (AnnenErfaring)
import Dato exposing (DatoPeriode(..), Måned(..), TilDato(..), År)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Textarea as Textarea
import FrontendModuler.ValgfriDatoInput as ValgfriDatoInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Meldinger.Melding as Melding exposing (Melding(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Task
import Validering



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , annenErfaringListe : List AnnenErfaring
    , debugStatus : DebugStatus
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List AnnenErfaring) FerdigAnimertMeldingsLogg


type AvsluttetGrunn
    = SlettetPåbegynt
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = RegistrerRolle RolleInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | SpørOmBrukerVilLeggeInnTidsperiode BeskrivelseInfo
    | RegistrerFraMåned FraDatoInfo
    | RegistrerFraÅr FraDatoInfo
    | RegistrerNåværende ValidertFraDatoInfo
    | RegistrerTilMåned TilDatoInfo
    | RegistrerTilÅr TilDatoInfo
    | VisOppsummering OppsummeringsType ValidertAnnenErfaringSkjema
    | EndreOpplysninger AnnenErfaringSkjema
    | BekreftSlettingAvPåbegynt ValidertAnnenErfaringSkjema
    | LagrerSkjema ValidertAnnenErfaringSkjema LagreStatus
    | LagringFeilet Http.Error ValidertAnnenErfaringSkjema
    | VenterPåAnimasjonFørFullføring (List AnnenErfaring) AvsluttetGrunn


type alias RolleInfo =
    { rolle : String
    , tillatÅViseFeilmeldingRolle : Bool
    }


type alias BeskrivelseInfo =
    { rolle : String
    , beskrivelse : String
    }


type alias FraDatoInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : String
    , tillatÅViseFeilmeldingÅr : Bool
    }


type alias ValidertFraDatoInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    }


type alias TilDatoInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    , tilMåned : Måned
    , tilÅr : String
    , tillatÅViseFeilmeldingÅr : Bool
    }


maxLengthBeskrivelse =
    2000


rolleTilBeskrivelse : String -> BeskrivelseInfo
rolleTilBeskrivelse rolle =
    { rolle = rolle
    , beskrivelse = ""
    }


beskrivelseTilFraDato : BeskrivelseInfo -> FraDatoInfo
beskrivelseTilFraDato input =
    { rolle = input.rolle
    , beskrivelse = input.beskrivelse
    , fraMåned = Januar
    , fraÅr = ""
    , tillatÅViseFeilmeldingÅr = False
    }


fraDatoTilTilDato : ValidertFraDatoInfo -> TilDatoInfo
fraDatoTilTilDato input =
    { rolle = input.rolle
    , beskrivelse = input.beskrivelse
    , fraMåned = input.fraMåned
    , fraÅr = input.fraÅr
    , tilMåned = Januar
    , tilÅr = ""
    , tillatÅViseFeilmeldingÅr = False
    }


beskrivelseTilSkjema : BeskrivelseInfo -> ValidertAnnenErfaringSkjema
beskrivelseTilSkjema info =
    Skjema.initValidertSkjemaUtenPeriode
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , id = Nothing
        }


fraDatoTilSkjema : ValidertFraDatoInfo -> ValidertAnnenErfaringSkjema
fraDatoTilSkjema info =
    Skjema.initValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , fraMåned = info.fraMåned
        , fraÅr = info.fraÅr
        , tilDato = Nåværende
        , id = Nothing
        }


tilDatoTilSkjema : TilDatoInfo -> År -> ValidertAnnenErfaringSkjema
tilDatoTilSkjema info tilÅr =
    Skjema.initValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , fraMåned = info.fraMåned
        , fraÅr = info.fraÅr
        , tilDato = Avsluttet info.tilMåned tilÅr
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = VilRegistrereRolle
    | OppdatererRolle String
    | VilRegistrereBeskrivelse
    | OppdatererBeskrivelse String
    | SvarerJaTilTidsperiode
    | SvarerNeiTilTidsperiode
    | FraMånedValgt Dato.Måned
    | VilRegistrereFraÅr
    | OppdatererFraÅr String
    | SvarerJaTilNåværende
    | SvarerNeiTilNåværende
    | TilMånedValgt Dato.Måned
    | VilRegistrereTilÅr
    | OppdatererTilÅr String
    | VilLagreAnnenErfaring
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | VilLagreEndretSkjema
    | AnnenErfaringLagret (Result Http.Error (List AnnenErfaring))
    | FerdigMedAnnenErfaring
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | FeltMisterFokus
    | ErrorLogget


type SkjemaEndring
    = Tekst Felt String
    | HarDatoerToggled
    | FraMåned String
    | NåværendeToggled
    | TilMåned String
    | FraÅrBlurred
    | TilÅrBlurred
    | RolleBlurred


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        VilRegistrereRolle ->
            case model.aktivSamtale of
                RegistrerRolle info ->
                    case Skjema.feilmeldingRolle info.rolle of
                        Just _ ->
                            ( { info | tillatÅViseFeilmeldingRolle = True }
                                |> RegistrerRolle
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( info.rolle
                                |> rolleTilBeskrivelse
                                |> RegistrerBeskrivelse
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererRolle string ->
            case model.aktivSamtale of
                RegistrerRolle info ->
                    ( { info | rolle = string }
                        |> RegistrerRolle
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse input ->
                    case Validering.feilmeldingMaxAntallTegn input.beskrivelse maxLengthBeskrivelse of
                        Nothing ->
                            ( input
                                |> SpørOmBrukerVilLeggeInnTidsperiode
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererBeskrivelse string ->
            case model.aktivSamtale of
                RegistrerBeskrivelse beskrivelse ->
                    ( { beskrivelse | beskrivelse = string }
                        |> RegistrerBeskrivelse
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerJaTilTidsperiode ->
            case model.aktivSamtale of
                SpørOmBrukerVilLeggeInnTidsperiode info ->
                    ( info
                        |> beskrivelseTilFraDato
                        |> RegistrerFraMåned
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerNeiTilTidsperiode ->
            case model.aktivSamtale of
                SpørOmBrukerVilLeggeInnTidsperiode info ->
                    ( info
                        |> beskrivelseTilSkjema
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FraMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerFraMåned fraDatoInfo ->
                    ( måned
                        |> setFraMåned fraDatoInfo
                        |> RegistrerFraÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereFraÅr ->
            case model.aktivSamtale of
                RegistrerFraÅr fraDatoInfo ->
                    case Dato.stringTilÅr fraDatoInfo.fraÅr of
                        Just fraÅr ->
                            ( { rolle = fraDatoInfo.rolle
                              , beskrivelse = fraDatoInfo.beskrivelse
                              , fraMåned = fraDatoInfo.fraMåned
                              , fraÅr = fraÅr
                              }
                                |> RegistrerNåværende
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fraDatoInfo | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerFraÅr
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererFraÅr string ->
            case model.aktivSamtale of
                RegistrerFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | fraÅr = string }
                        |> RegistrerFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerJaTilNåværende ->
            case model.aktivSamtale of
                RegistrerNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> fraDatoTilSkjema
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerNeiTilNåværende ->
            case model.aktivSamtale of
                RegistrerNåværende fraDatoInfo ->
                    ( fraDatoInfo
                        |> fraDatoTilTilDato
                        |> RegistrerTilMåned
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        TilMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerTilMåned tilDatoInfo ->
                    ( { tilDatoInfo | tilMåned = måned }
                        |> RegistrerTilÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereTilÅr ->
            case model.aktivSamtale of
                RegistrerTilÅr tilDatoInfo ->
                    case Dato.stringTilÅr tilDatoInfo.tilÅr of
                        Just tilÅr ->
                            ( tilDatoTilSkjema tilDatoInfo tilÅr
                                |> VisOppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerTilÅr
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererTilÅr string ->
            case model.aktivSamtale of
                RegistrerTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | tilÅr = string }
                        |> RegistrerTilÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FeltMisterFokus ->
            case model.aktivSamtale of
                RegistrerFraÅr fraDatoInfo ->
                    ( { fraDatoInfo | tillatÅViseFeilmeldingÅr = True }
                        |> RegistrerFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | tillatÅViseFeilmeldingÅr = True }
                        |> RegistrerTilÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerRolle rolleInfo ->
                    ( { rolleInfo | tillatÅViseFeilmeldingRolle = True }
                        |> RegistrerRolle
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilEndreOpplysninger ->
            case model.aktivSamtale of
                VisOppsummering _ validertAnnenErfaringSkjema ->
                    updateEtterVilEndreSkjema model msg validertAnnenErfaringSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger annenErfaringSkjema ->
                    ( annenErfaringSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSlettePåbegynt ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
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
                    ( VenterPåAnimasjonFørFullføring model.annenErfaringListe SlettetPåbegynt
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AngrerSlettPåbegynt ->
            case model.aktivSamtale of
                BekreftSlettingAvPåbegynt skjema ->
                    ( VisOppsummering AvbrøtSletting skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreEndretSkjema ->
            case model.aktivSamtale of
                EndreOpplysninger skjema ->
                    case Skjema.valider skjema of
                        Just validertSkjema ->
                            ( validertSkjema
                                |> VisOppsummering EtterEndring
                                |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.tillatÅViseAlleFeilmeldinger
                                |> EndreOpplysninger
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreAnnenErfaring ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , Api.postAnnenErfaring AnnenErfaringLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        AnnenErfaringLagret result ->
            case model.aktivSamtale of
                LagrerSkjema skjema lagreStatus ->
                    case result of
                        Ok annenErfaringer ->
                            let
                                oppdatertMeldingslogg =
                                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Bra. Nå har du lagt til denne erfaringen." ] ]

                                    else
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Bra. Nå har du lagt til denne erfaringen." ] ]
                            in
                            ( VenterPåAnimasjonFørFullføring annenErfaringer AnnenAvslutning
                                |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.postAnnenErfaring AnnenErfaringLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre annen erfaring" error
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
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre annen erfaring" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FerdigMedAnnenErfaring ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( VenterPåAnimasjonFørFullføring model.annenErfaringListe AnnenAvslutning
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagrerSkjema skjema lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSkjema skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.postAnnenErfaring AnnenErfaringLagret skjema
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

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )


setFraMåned : FraDatoInfo -> Dato.Måned -> FraDatoInfo
setFraMåned fraDatoInfo måned =
    { fraDatoInfo | fraMåned = måned }


oppdaterSkjema : SkjemaEndring -> AnnenErfaringSkjema -> AnnenErfaringSkjema
oppdaterSkjema endring skjema =
    case endring of
        Tekst felt innhold ->
            Skjema.oppdaterTekstFelt felt innhold skjema

        HarDatoerToggled ->
            Skjema.toggleHarDatoer skjema

        FraMåned månedString ->
            månedString
                |> Dato.stringTilMaybeMåned
                |> Skjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Dato.stringTilMaybeMåned
                |> Skjema.oppdaterTilMåned skjema

        NåværendeToggled ->
            Skjema.toggleNavarende skjema

        FraÅrBlurred ->
            Skjema.tillatÅViseFeilmeldingFraÅr skjema

        TilÅrBlurred ->
            Skjema.tillatÅViseFeilmeldingTilÅr skjema

        RolleBlurred ->
            Skjema.tillatÅViseFeilmeldingRolle skjema


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring annenErfaringListe _ ->
                    Ferdig annenErfaringListe ferdigAnimertSamtale

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


updateEtterVilEndreSkjema : ModelInfo -> Msg -> ValidertAnnenErfaringSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model msg skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertAnnenErfaringSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Api.postAnnenErfaring AnnenErfaringLagret skjema
    )
        |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


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
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                    ManueltSvar melding ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar melding
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                    UtenSvar ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg annenErfaringSeksjon =
    case annenErfaringSeksjon of
        RegistrerRolle _ ->
            [ Melding.spørsmål [ "Så bra at du har mer erfaring. Hvilken rolle har du hatt?" ]
            , Melding.spørsmål
                [ "Har du jobbet som fotballtrener, besøksvenn, eller noe helt annet?" ]
            ]

        RegistrerBeskrivelse _ ->
            [ Melding.spørsmål [ "Hva var jobben din?" ]
            ]

        SpørOmBrukerVilLeggeInnTidsperiode _ ->
            [ Melding.spørsmål [ "Det kan være nyttig for en arbeidsgiver å vite hvor lenge du har hatt rollen. Vil du legge inn tidsperiode?" ]
            ]

        RegistrerFraMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned begynte du?" ]
            ]

        RegistrerFraÅr _ ->
            [ Melding.spørsmål [ "Hvilket år begynte du?" ]
            ]

        RegistrerNåværende info ->
            [ Melding.spørsmål [ "Jobber du fremdeles som «" ++ info.rolle ++ "»?" ] ]

        RegistrerTilMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned sluttet du?" ]
            ]

        RegistrerTilÅr _ ->
            [ Melding.spørsmål [ "Hvilket år sluttet du?" ]
            ]

        VisOppsummering oppsummeringsType skjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Da sletter jeg ikke erfaringen." ]
                    , oppsummeringsSpørsmål skjema
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål skjema ]

        EndreOpplysninger _ ->
            [ Melding.spørsmål [ "Nå kan du endre informasjonen." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne erfaringen?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre annen erfaring" } ]

        VenterPåAnimasjonFørFullføring _ avsluttetGrunn ->
            case avsluttetGrunn of
                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg slettet erfaringen. Vil du legge inn flere kategorier?" ] ]

                AnnenAvslutning ->
                    [ Melding.spørsmål [ "Vil du legge inn flere kategorier?" ] ]


validertSkjemaTilSetninger : ValidertAnnenErfaringSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    case Skjema.periode validertSkjema of
        Oppgitt fraMåned_ fraÅr_ tilDato ->
            [ Dato.periodeTilString fraMåned_ fraÅr_ tilDato
            , Melding.tomLinje
            , "Rolle: " ++ Skjema.innholdTekstFelt Rolle skjema
            , "Beskrivelse: " ++ Skjema.innholdTekstFelt Beskrivelse skjema
            ]

        IkkeOppgitt ->
            [ "Rolle: " ++ Skjema.innholdTekstFelt Rolle skjema
            , "Beskrivelse: " ++ Skjema.innholdTekstFelt Beskrivelse skjema
            ]


oppsummeringsSpørsmål : ValidertAnnenErfaringSkjema -> Melding
oppsummeringsSpørsmål skjema =
    [ [ "Du har lagt inn dette:"
      , Melding.tomLinje
      ]
    , validertSkjemaTilSetninger skjema
    , [ Melding.tomLinje
      , "Er informasjonen riktig?"
      ]
    ]
        |> List.concat
        |> Melding.spørsmål


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerRolle _ ->
            settFokusCmd RolleId

        RegistrerBeskrivelse _ ->
            settFokusCmd BeskrivelseId

        RegistrerFraÅr _ ->
            settFokusCmd FraÅrId

        RegistrerTilÅr _ ->
            settFokusCmd TilÅrId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt



--- VIEW ---


type InputId
    = RolleId
    | BeskrivelseId
    | FraÅrId
    | TilÅrId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        RolleId ->
            "annenErfaring-rolle-id"

        BeskrivelseId ->
            "annenErfaring-beskrivelse-id"

        FraÅrId ->
            "annenErfaring-fraÅr-id"

        TilÅrId ->
            "annenErfaring-tilÅr-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            RegistrerRolle info ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereRolle
                    (info.rolle
                        |> Input.input { label = "Rolle", msg = OppdatererRolle }
                        |> Input.withOnEnter VilRegistrereRolle
                        |> Input.withId (inputIdTilString RolleId)
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withMaybeFeilmelding
                            (info.rolle
                                |> Skjema.feilmeldingRolle
                                |> maybeHvisTrue info.tillatÅViseFeilmeldingRolle
                            )
                        |> Input.withErObligatorisk
                    )

            RegistrerBeskrivelse info ->
                BrukerInput.textareaMedGåVidereKnapp VilRegistrereBeskrivelse
                    (info.beskrivelse
                        |> Textarea.textarea { label = "Beskriv oppgavene dine", msg = OppdatererBeskrivelse }
                        |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn info.beskrivelse maxLengthBeskrivelse)
                        |> Textarea.withId (inputIdTilString BeskrivelseId)
                    )

            SpørOmBrukerVilLeggeInnTidsperiode _ ->
                BrukerInput.knapper Flytende
                    [ "Ja, det vil jeg"
                        |> Knapp.knapp SvarerJaTilTidsperiode
                    , "Nei, det vil jeg ikke"
                        |> Knapp.knapp SvarerNeiTilTidsperiode
                    ]

            RegistrerFraMåned _ ->
                BrukerInput.månedKnapper FraMånedValgt

            RegistrerFraÅr fraDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereFraÅr
                    (fraDatoInfo.fraÅr
                        |> Input.input { label = "År", msg = OppdatererFraÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereFraÅr
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withId (inputIdTilString FraÅrId)
                        |> Input.withMaybeFeilmelding
                            (fraDatoInfo.fraÅr
                                |> Dato.feilmeldingÅr
                                |> maybeHvisTrue fraDatoInfo.tillatÅViseFeilmeldingÅr
                            )
                        |> Input.withErObligatorisk
                    )

            RegistrerNåværende _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp SvarerJaTilNåværende "Ja"
                    , Knapp.knapp SvarerNeiTilNåværende "Nei"
                    ]

            RegistrerTilMåned _ ->
                BrukerInput.månedKnapper TilMånedValgt

            RegistrerTilÅr tilDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereTilÅr
                    (tilDatoInfo.tilÅr
                        |> Input.input { label = "År", msg = OppdatererTilÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereTilÅr
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withId (inputIdTilString TilÅrId)
                        |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilDatoInfo.tillatÅViseFeilmeldingÅr) tilDatoInfo.tilÅr)
                    )

            VisOppsummering _ _ ->
                viewBekreftOppsummering

            EndreOpplysninger skjema ->
                BrukerInput.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                    [ skjema
                        |> Skjema.innholdTekstFelt Rolle
                        |> Input.input { label = "Rolle", msg = Tekst Rolle >> SkjemaEndret }
                        |> Input.withMaybeFeilmelding (Skjema.feilmeldingRolleHvisSynlig skjema)
                        |> Input.withOnBlur (SkjemaEndret RolleBlurred)
                        |> Input.withErObligatorisk
                        |> Input.toHtml
                    , skjema
                        |> Skjema.innholdTekstFelt Beskrivelse
                        |> Textarea.textarea { label = "Beskrivelse", msg = Tekst Beskrivelse >> SkjemaEndret }
                        |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Beskrivelse skjema) maxLengthBeskrivelse)
                        |> Textarea.toHtml
                    , skjema
                        |> Skjema.harDatoer
                        |> Checkbox.checkbox "Jeg vil legge inn tidsperiode" (SkjemaEndret HarDatoerToggled)
                        |> Checkbox.toHtml
                    , if Skjema.harDatoer skjema then
                        viewDatoPeriode skjema

                      else
                        text ""
                    ]

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                    ]

            LagrerSkjema _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    ErrorHåndtering.GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedAnnenErfaring "Gå videre"
                            ]

                    ErrorHåndtering.PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreAnnenErfaring "Prøv igjen"
                            , Knapp.knapp FerdigMedAnnenErfaring "Gå videre"
                            ]

                    ErrorHåndtering.LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


viewBekreftOppsummering : BrukerInput Msg
viewBekreftOppsummering =
    BrukerInput.knapper Kolonne
        [ Knapp.knapp VilLagreAnnenErfaring "Ja, det er riktig"
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
        , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
        ]


viewDatoPeriode : AnnenErfaringSkjema -> Html Msg
viewDatoPeriode skjema =
    div []
        [ div [ class "DatoInput-fra-til-rad" ]
            [ ValgfriDatoInput.datoInput
                { label = "Når begynte du?"
                , onMånedChange = FraMåned >> SkjemaEndret
                , måned = Skjema.fraMåned skjema
                , onÅrChange = Tekst FraÅr >> SkjemaEndret
                , år = Skjema.innholdTekstFelt FraÅr skjema
                }
                |> ValgfriDatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFraÅr skjema)
                |> ValgfriDatoInput.withMaybeFeilmeldingMåned (Skjema.feilmeldingFraMåned skjema)
                |> ValgfriDatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
                |> ValgfriDatoInput.withErObligatorisk
                |> ValgfriDatoInput.toHtml
            , if not (Skjema.nåværende skjema) then
                ValgfriDatoInput.datoInput
                    { label = "Når sluttet du?"
                    , onMånedChange = TilMåned >> SkjemaEndret
                    , måned = Skjema.tilMåned skjema
                    , onÅrChange = Tekst TilÅr >> SkjemaEndret
                    , år = Skjema.innholdTekstFelt TilÅr skjema
                    }
                    |> ValgfriDatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingTilÅr skjema)
                    |> ValgfriDatoInput.withMaybeFeilmeldingMåned (Skjema.feilmeldingTilMåned skjema)
                    |> ValgfriDatoInput.withOnBlurÅr (SkjemaEndret TilÅrBlurred)
                    |> ValgfriDatoInput.withErObligatorisk
                    |> ValgfriDatoInput.toHtml

              else
                text ""
            ]
        , skjema
            |> Skjema.nåværende
            |> Checkbox.checkbox "Jeg holder fortsatt på" (SkjemaEndret NåværendeToggled)
            |> Checkbox.withClass "blokk-m"
            |> Checkbox.toHtml
        ]


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List AnnenErfaring -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg annenErfaringListe =
    let
        aktivSamtale =
            RegistrerRolle { rolle = "", tillatÅViseFeilmeldingRolle = False }
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (MeldingsLogg.tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , annenErfaringListe = annenErfaringListe
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
