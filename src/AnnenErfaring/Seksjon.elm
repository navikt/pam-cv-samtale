module AnnenErfaring.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , init
    , meldingsLogg
    , sistLagret
    , subscriptions
    , update
    , viewBrukerInput
    )

import AnnenErfaring.AnnenErfaring as AnnenErfaring exposing (AnnenErfaring)
import AnnenErfaring.Skjema as Skjema exposing (AnnenErfaringSkjema, Felt(..), ValidertAnnenErfaringSkjema)
import Api
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Dato.Dato as Dato exposing (DatoPeriode(..), TilDato(..), År)
import Dato.Maned as Måned exposing (Måned(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp exposing (BrukerInputMedGåVidereKnapp)
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
import Tid exposing (nyesteSistLagretVerdi)
import Time exposing (Posix)
import Validering



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , annenErfaringListe : List AnnenErfaring
    , debugStatus : DebugStatus
    , sistLagretFraForrigeSeksjon : Posix
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


sistLagret : Model -> Posix
sistLagret (Model model) =
    let
        sistLagretListe =
            List.map (\x -> Time.posixToMillis (AnnenErfaring.sistEndretDato x)) model.annenErfaringListe
    in
    nyesteSistLagretVerdi sistLagretListe model.sistLagretFraForrigeSeksjon


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix (List AnnenErfaring) FerdigAnimertMeldingsLogg


type AvsluttetGrunn
    = AvbruttPåbegynt
    | SlettetPåbegynt
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = RegistrerRolle RolleInfo
    | RegistrerBeskrivelse Bool BeskrivelseInfo
    | SpørOmBrukerVilLeggeInnTidsperiode BeskrivelseInfo
    | RegistrerFraÅr FraÅrInfo
    | RegistrerFraMåned FraMånedInfo
    | RegistrerNåværende NåværendeInfo
    | RegistrerTilÅr TilÅrInfo
    | RegistrerTilMåned TilMånedInfo
    | VisOppsummering OppsummeringsType ValidertAnnenErfaringSkjema
    | EndreOpplysninger AnnenErfaringSkjema
    | BekreftSlettingAvPåbegynt ValidertAnnenErfaringSkjema
    | LagrerSkjema ValidertAnnenErfaringSkjema LagreStatus
    | LagringFeilet Http.Error ValidertAnnenErfaringSkjema
    | BekreftAvbrytingAvRegistreringen Samtale
    | VenterPåAnimasjonFørFullføring (List AnnenErfaring) AvsluttetGrunn


type alias RolleInfo =
    { rolle : String
    , tillatÅViseFeilmeldingRolle : Bool
    }


type alias BeskrivelseInfo =
    { rolle : String
    , beskrivelse : String
    }


type alias FraÅrInfo =
    { rolle : String
    , beskrivelse : String
    , fraÅr : String
    , tillatÅViseFeilmeldingÅr : Bool
    }


type alias FraMånedInfo =
    { rolle : String
    , beskrivelse : String
    , fraÅr : År
    }


type alias NåværendeInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    }


type alias TilÅrInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    , tilÅr : String
    , tillatÅViseFeilmeldingÅr : Bool
    }


type alias TilMånedInfo =
    { rolle : String
    , beskrivelse : String
    , fraMåned : Måned
    , fraÅr : År
    , tilÅr : År
    }


maxLengthBeskrivelse =
    2000


rolleTilBeskrivelseInfo : String -> BeskrivelseInfo
rolleTilBeskrivelseInfo rolle =
    { rolle = rolle
    , beskrivelse = ""
    }


beskrivelseTilFraÅr : BeskrivelseInfo -> FraÅrInfo
beskrivelseTilFraÅr info =
    { rolle = info.rolle
    , beskrivelse = info.beskrivelse
    , fraÅr = ""
    , tillatÅViseFeilmeldingÅr = False
    }


nåværendeTilTilÅr : NåværendeInfo -> TilÅrInfo
nåværendeTilTilÅr info =
    { rolle = info.rolle
    , beskrivelse = info.beskrivelse
    , fraMåned = info.fraMåned
    , fraÅr = info.fraÅr
    , tilÅr = ""
    , tillatÅViseFeilmeldingÅr = False
    }


fraMånedTilNåværende : FraMånedInfo -> Måned -> NåværendeInfo
fraMånedTilNåværende info fraMåned =
    { rolle = info.rolle
    , beskrivelse = info.beskrivelse
    , fraMåned = fraMåned
    , fraÅr = info.fraÅr
    }


beskrivelseInfoTilSkjema : BeskrivelseInfo -> ValidertAnnenErfaringSkjema
beskrivelseInfoTilSkjema info =
    Skjema.initValidertSkjemaUtenPeriode
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , id = Nothing
        }


nåværendeInfoTilSkjema : NåværendeInfo -> ValidertAnnenErfaringSkjema
nåværendeInfoTilSkjema info =
    Skjema.initValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , fraMåned = info.fraMåned
        , fraÅr = info.fraÅr
        , tilDato = Nåværende
        , id = Nothing
        }


tilMånedTilSkjema : TilMånedInfo -> Måned -> ValidertAnnenErfaringSkjema
tilMånedTilSkjema info tilMåned =
    Skjema.initValidertSkjema
        { rolle = info.rolle
        , beskrivelse = info.beskrivelse
        , fraMåned = info.fraMåned
        , fraÅr = info.fraÅr
        , tilDato = Avsluttet tilMåned info.tilÅr
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = VilRegistrereRolle
    | OppdatererRolle String
    | VilSeEksempel
    | VilRegistrereBeskrivelse
    | OppdatererBeskrivelse String
    | SvarerJaTilTidsperiode
    | SvarerNeiTilTidsperiode
    | FraMånedValgt Måned
    | VilRegistrereFraÅr
    | OppdatererFraÅr String
    | SvarerJaTilNåværende
    | SvarerNeiTilNåværende
    | TilMånedValgt Måned
    | VilRegistrereTilÅr
    | OppdatererTilÅr String
    | VilLagreAnnenErfaring
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | VilAvbryteRegistreringen
    | BekrefterAvbrytingAvRegistrering
    | VilIkkeAvbryteRegistreringen
    | VilLagreEndretSkjema
    | AnnenErfaringLagret (Result Http.Error (List AnnenErfaring))
    | FerdigMedAnnenErfaring
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | FeltMisterFokus
    | TimeoutEtterAtFeltMistetFokus
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
                                |> rolleTilBeskrivelseInfo
                                |> RegistrerBeskrivelse True
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

        VilSeEksempel ->
            case model.aktivSamtale of
                RegistrerBeskrivelse _ info ->
                    let
                        oppdatertMeldingslogg =
                            model.seksjonsMeldingsLogg
                                |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                |> MeldingsLogg.leggTilSpørsmål eksemplerPåAnnenErfaring
                    in
                    IkkeFerdig
                        ( info
                            |> RegistrerBeskrivelse False
                            |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse _ input ->
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
                RegistrerBeskrivelse medEksempelKnapp beskrivelse ->
                    ( { beskrivelse | beskrivelse = string }
                        |> RegistrerBeskrivelse medEksempelKnapp
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
                        |> beskrivelseTilFraÅr
                        |> RegistrerFraÅr
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
                        |> beskrivelseInfoTilSkjema
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererFraÅr string ->
            case model.aktivSamtale of
                RegistrerFraÅr fraÅrInfo ->
                    ( { fraÅrInfo | fraÅr = string }
                        |> RegistrerFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereFraÅr ->
            case model.aktivSamtale of
                RegistrerFraÅr info ->
                    case Dato.stringTilÅr info.fraÅr of
                        Just fraÅr ->
                            ( { rolle = info.rolle
                              , beskrivelse = info.beskrivelse
                              , fraÅr = fraÅr
                              }
                                |> RegistrerFraMåned
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { info | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerFraÅr
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FraMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerFraMåned fraMånedInfo ->
                    ( måned
                        |> fraMånedTilNåværende fraMånedInfo
                        |> RegistrerNåværende
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerJaTilNåværende ->
            case model.aktivSamtale of
                RegistrerNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> nåværendeInfoTilSkjema
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerNeiTilNåværende ->
            case model.aktivSamtale of
                RegistrerNåværende nåværendeInfo ->
                    ( nåværendeInfo
                        |> nåværendeTilTilÅr
                        |> RegistrerTilÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererTilÅr string ->
            case model.aktivSamtale of
                RegistrerTilÅr tilÅrInfo ->
                    ( { tilÅrInfo | tilÅr = string }
                        |> RegistrerTilÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereTilÅr ->
            case model.aktivSamtale of
                RegistrerTilÅr tilÅrInfo ->
                    case Dato.stringTilÅr tilÅrInfo.tilÅr of
                        Just tilÅr ->
                            ( { rolle = tilÅrInfo.rolle
                              , beskrivelse = tilÅrInfo.beskrivelse
                              , fraMåned = tilÅrInfo.fraMåned
                              , fraÅr = tilÅrInfo.fraÅr
                              , tilÅr = tilÅr
                              }
                                |> RegistrerTilMåned
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilÅrInfo | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerTilÅr
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        TilMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerTilMåned tilMånedInfo ->
                    ( måned
                        |> tilMånedTilSkjema tilMånedInfo
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FeltMisterFokus ->
            IkkeFerdig ( Model model, mistetFokusCmd )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                RegistrerFraÅr info ->
                    ( { info | tillatÅViseFeilmeldingÅr = True }
                        |> RegistrerFraÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerTilÅr tilÅrInfo ->
                    ( { tilÅrInfo | tillatÅViseFeilmeldingÅr = True }
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

        VilAvbryteRegistreringen ->
            case model.aktivSamtale of
                RegistrerRolle _ ->
                    avbrytRegistrering model msg

                _ ->
                    ( model.aktivSamtale
                        |> BekreftAvbrytingAvRegistreringen
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

        BekrefterAvbrytingAvRegistrering ->
            avbrytRegistrering model msg

        VilIkkeAvbryteRegistreringen ->
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
                                            , samtaleTilMeldingsLogg samtaleStegFørAvbryting
                                            ]
                                        )
                        }
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
                    , Api.opprettAnnenErfaring AnnenErfaringLagret skjema
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
                                |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg, annenErfaringListe = annenErfaringer } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , Api.opprettAnnenErfaring AnnenErfaringLagret skjema
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
                                    , Api.opprettAnnenErfaring AnnenErfaringLagret skjema
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


avbrytRegistrering : ModelInfo -> Msg -> SamtaleStatus
avbrytRegistrering model msg =
    ( AvbruttPåbegynt
        |> VenterPåAnimasjonFørFullføring model.annenErfaringListe
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


oppdaterSkjema : SkjemaEndring -> AnnenErfaringSkjema -> AnnenErfaringSkjema
oppdaterSkjema endring skjema =
    case endring of
        Tekst felt innhold ->
            Skjema.oppdaterTekstFelt felt innhold skjema

        HarDatoerToggled ->
            Skjema.toggleHarDatoer skjema

        FraMåned månedString ->
            månedString
                |> Måned.fraString
                |> Skjema.oppdaterFraMåned skjema

        TilMåned månedString ->
            månedString
                |> Måned.fraString
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
                    Ferdig (sistLagret (Model model)) annenErfaringListe ferdigAnimertSamtale

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
    , Api.opprettAnnenErfaring AnnenErfaringLagret skjema
    )
        |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


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
            , Melding.spørsmål [ "Har du jobbet som fotballtrener, besøksvenn, eller noe helt annet?" ]
            ]

        RegistrerBeskrivelse _ _ ->
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
                    [ Melding.spørsmål [ "Ok, da lar jeg erfaringen stå." ]
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

        BekreftAvbrytingAvRegistreringen _ ->
            [ Melding.spørsmål [ "Hvis du avbryter, blir ikke erfaringen lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

        VenterPåAnimasjonFørFullføring _ avsluttetGrunn ->
            case avsluttetGrunn of
                AvbruttPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg avbrutt. Vil du legge inn flere kategorier?" ] ]

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

        RegistrerBeskrivelse _ _ ->
            settFokusCmd BeskrivelseId

        SpørOmBrukerVilLeggeInnTidsperiode _ ->
            settFokusCmd LeggTilPeriodeId

        RegistrerFraMåned _ ->
            settFokusCmd FraMånedId

        RegistrerFraÅr _ ->
            settFokusCmd FraÅrId

        RegistrerNåværende _ ->
            settFokusCmd NåværendeId

        RegistrerTilMåned _ ->
            settFokusCmd TilMånedId

        RegistrerTilÅr _ ->
            settFokusCmd TilÅrId

        EndreOpplysninger _ ->
            settFokusCmd RolleId

        VisOppsummering _ _ ->
            settFokusCmd BekreftOppsummeringId

        LagringFeilet _ _ ->
            settFokusCmd LagringFeiletActionId

        BekreftSlettingAvPåbegynt _ ->
            settFokusCmd SlettePåbegyntId

        BekreftAvbrytingAvRegistreringen _ ->
            settFokusCmd AvbrytSlettingId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


eksemplerPåAnnenErfaring : List Melding
eksemplerPåAnnenErfaring =
    [ Melding.eksempelMedTittel "Eksempel 1:" [ "5 års erfaring som fotballtrener for jente- og guttelag i Moss FK." ]
    , Melding.eksempelMedTittel "Eksempel 2:" [ "Vært besøksvenn i Røde Kors, 2 timer per uke. Har besøkt en eldre, enslig mann og bidratt til sosiale aktiviteter." ]
    , Melding.eksempelMedTittel "Eksempel 3:" [ "2 års erfaring som kasserer i Elgveien borettslag." ]
    ]



--- VIEW ---


type InputId
    = RolleId
    | BeskrivelseId
    | LeggTilPeriodeId
    | FraMånedId
    | FraÅrId
    | NåværendeId
    | TilMånedId
    | TilÅrId
    | BekreftOppsummeringId
    | SlettePåbegyntId
    | LagringFeiletActionId
    | AvbrytSlettingId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        RolleId ->
            "annenErfaring-rolle-id"

        BeskrivelseId ->
            "annenErfaring-beskrivelse-id"

        LeggTilPeriodeId ->
            "annenErfaring-periode-id"

        FraMånedId ->
            "annenErfaring-fraMåned-id"

        FraÅrId ->
            "annenErfaring-fraÅr-id"

        NåværendeId ->
            "annenErfaring-nåværende-id"

        TilMånedId ->
            "annenErfaring-tilMåned-id"

        TilÅrId ->
            "annenErfaring-tilÅr-id"

        BekreftOppsummeringId ->
            "annenErfaring-bekreft-oppsummering-id"

        SlettePåbegyntId ->
            "annenErfaring-slett-påbegynt-id"

        LagringFeiletActionId ->
            "annenErfaring-lagring-feilet-id"

        AvbrytSlettingId ->
            "annenErfaring-avbrytt-slett-id"


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
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = VilAvbryteRegistreringen, onGåVidere = VilRegistrereRolle }
                    (info.rolle
                        |> Input.input { label = "Rolle", msg = OppdatererRolle }
                        |> Input.withOnEnter VilRegistrereRolle
                        |> Input.withId (inputIdTilString RolleId)
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withFeilmelding
                            (info.rolle
                                |> Skjema.feilmeldingRolle
                                |> maybeHvisTrue info.tillatÅViseFeilmeldingRolle
                            )
                        |> Input.withErObligatorisk
                    )

            RegistrerBeskrivelse medEksempelKnapp info ->
                info.beskrivelse
                    |> Textarea.textarea { label = "Beskriv oppgavene dine", msg = OppdatererBeskrivelse }
                    |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn info.beskrivelse maxLengthBeskrivelse)
                    |> Textarea.withId (inputIdTilString BeskrivelseId)
                    |> BrukerInputMedGåVidereKnapp.textarea VilRegistrereBeskrivelse
                    |> BrukerInputMedGåVidereKnapp.withVisEksempelKnapp medEksempelKnapp VilSeEksempel
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp VilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            SpørOmBrukerVilLeggeInnTidsperiode _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp SvarerJaTilTidsperiode "Ja, det vil jeg"
                        |> Knapp.withId (inputIdTilString LeggTilPeriodeId)
                    , Knapp.knapp SvarerNeiTilTidsperiode "Nei, det vil jeg ikke"
                    ]

            RegistrerFraMåned _ ->
                BrukerInput.månedKnapper
                    { onAvbryt = VilAvbryteRegistreringen
                    , onMånedValg = FraMånedValgt
                    , fokusId = inputIdTilString FraMånedId
                    }

            RegistrerFraÅr info ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = VilAvbryteRegistreringen, onGåVidere = VilRegistrereFraÅr }
                    (info.fraÅr
                        |> Input.input { label = "År", msg = OppdatererFraÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereFraÅr
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withId (inputIdTilString FraÅrId)
                        |> Input.withFeilmelding
                            (info.fraÅr
                                |> Dato.feilmeldingÅr
                                |> maybeHvisTrue info.tillatÅViseFeilmeldingÅr
                            )
                        |> Input.withErObligatorisk
                    )

            RegistrerNåværende _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp SvarerJaTilNåværende "Ja"
                        |> Knapp.withId (inputIdTilString NåværendeId)
                    , Knapp.knapp SvarerNeiTilNåværende "Nei"
                    ]

            RegistrerTilMåned _ ->
                BrukerInput.månedKnapper
                    { onAvbryt = VilAvbryteRegistreringen
                    , onMånedValg = TilMånedValgt
                    , fokusId = inputIdTilString TilMånedId
                    }

            RegistrerTilÅr tilÅrInfo ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = VilAvbryteRegistreringen, onGåVidere = VilRegistrereTilÅr }
                    (tilÅrInfo.tilÅr
                        |> Input.input { label = "År", msg = OppdatererTilÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereTilÅr
                        |> Input.withOnBlur FeltMisterFokus
                        |> Input.withId (inputIdTilString TilÅrId)
                        |> Input.withFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilÅrInfo.tillatÅViseFeilmeldingÅr) tilÅrInfo.tilÅr)
                        |> Input.withErObligatorisk
                    )

            VisOppsummering _ _ ->
                viewBekreftOppsummering

            EndreOpplysninger skjema ->
                BrukerInput.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                    [ skjema
                        |> Skjema.innholdTekstFelt Rolle
                        |> Input.input { label = "Rolle", msg = Tekst Rolle >> SkjemaEndret }
                        |> Input.withFeilmelding (Skjema.feilmeldingRolleHvisSynlig skjema)
                        |> Input.withOnBlur (SkjemaEndret RolleBlurred)
                        |> Input.withErObligatorisk
                        |> Input.withId (inputIdTilString RolleId)
                        |> Input.toHtml
                    , skjema
                        |> Skjema.innholdTekstFelt Beskrivelse
                        |> Textarea.textarea { label = "Beskrivelse", msg = Tekst Beskrivelse >> SkjemaEndret }
                        |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn (Skjema.innholdTekstFelt Beskrivelse skjema) maxLengthBeskrivelse)
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
                        |> Knapp.withId (inputIdTilString SlettePåbegyntId)
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
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    ErrorHåndtering.PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreAnnenErfaring "Prøv igjen"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp FerdigMedAnnenErfaring "Gå videre"
                            ]

                    ErrorHåndtering.LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            BekreftAvbrytingAvRegistreringen _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterAvbrytingAvRegistrering "Ja, jeg vil avbryte"
                        |> Knapp.withId (inputIdTilString AvbrytSlettingId)
                    , Knapp.knapp VilIkkeAvbryteRegistreringen "Nei, jeg vil fortsette"
                    ]

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


viewBekreftOppsummering : BrukerInput Msg
viewBekreftOppsummering =
    BrukerInput.knapper Kolonne
        [ Knapp.knapp VilLagreAnnenErfaring "Ja, det er riktig"
            |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
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
                |> ValgfriDatoInput.withFeilmeldingÅr (Skjema.feilmeldingFraÅr skjema)
                |> ValgfriDatoInput.withFeilmeldingMåned (Skjema.feilmeldingFraMåned skjema)
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
                    |> ValgfriDatoInput.withFeilmeldingÅr (Skjema.feilmeldingTilÅr skjema)
                    |> ValgfriDatoInput.withFeilmeldingMåned (Skjema.feilmeldingTilMåned skjema)
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


init : DebugStatus -> Posix -> FerdigAnimertMeldingsLogg -> List AnnenErfaring -> ( Model, Cmd Msg )
init debugStatus sistLagretFraForrigeSeksjon gammelMeldingsLogg annenErfaringListe =
    let
        aktivSamtale =
            RegistrerRolle { rolle = "", tillatÅViseFeilmeldingRolle = False }
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , aktivSamtale = aktivSamtale
        , annenErfaringListe = annenErfaringListe
        , debugStatus = debugStatus
        , sistLagretFraForrigeSeksjon = sistLagretFraForrigeSeksjon
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
