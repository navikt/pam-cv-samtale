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
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Textarea as Textarea
import FrontendModuler.ValgfriDatoInput as ValgfriDatoInput
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Process
import SamtaleAnimasjon
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


type Samtale
    = RegistrerRolle RolleInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | SpørOmBrukerVilLeggeInnTidsperiode BeskrivelseInfo
    | RegistrerFraMåned FraDatoInfo
    | RegistrerFraÅr FraDatoInfo
    | RegistrerNåværende ValidertFraDatoInfo
    | RegistrerTilMåned TilDatoInfo
    | RegistrerTilÅr TilDatoInfo
    | VisOppsummering ValidertAnnenErfaringSkjema
    | EndreOpplysninger AnnenErfaringSkjema
    | VisOppsummeringEtterEndring ValidertAnnenErfaringSkjema
    | LagrerSkjema ValidertAnnenErfaringSkjema LagreStatus
    | LagringFeilet Http.Error ValidertAnnenErfaringSkjema
    | VenterPåAnimasjonFørFullføring (List AnnenErfaring)


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
    | VilLagreEndretSkjema
    | AnnenErfaringLagret (Result Http.Error (List AnnenErfaring))
    | FerdigMedAnnenErfaring
    | WindowEndrerVisibility Visibility
    | StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
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
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( info.rolle
                                |> rolleTilBeskrivelse
                                |> RegistrerBeskrivelse
                                |> nesteSamtaleSteg model (Melding.svar [ info.rolle ])
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
                        |> oppdaterSamtaleSteg model
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
                                |> nesteSamtaleSteg model (Melding.svar [ input.beskrivelse ])
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
                        |> oppdaterSamtaleSteg model
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja, det vil jeg" ])
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
                        |> VisOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei, det vil jeg ikke" ])
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
                        |> nesteSamtaleSteg model
                            (Melding.svar [ måned |> Dato.månedTilString ])
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
                                |> nesteSamtaleSteg model (Melding.svar [ fraDatoInfo.fraÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fraDatoInfo | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerFraÅr
                                |> oppdaterSamtaleSteg model
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
                        |> oppdaterSamtaleSteg model
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
                        |> VisOppsummering
                        |> nesteSamtaleSteg model (Melding.svar [ "Ja" ])
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
                        |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
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
                        |> nesteSamtaleSteg model
                            (Melding.svar [ måned |> Dato.månedTilString ])
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
                                |> VisOppsummering
                                |> nesteSamtaleSteg model (Melding.svar [ tilDatoInfo.tilÅr ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { tilDatoInfo | tillatÅViseFeilmeldingÅr = True }
                                |> RegistrerTilÅr
                                |> oppdaterSamtaleSteg model
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
                        |> oppdaterSamtaleSteg model
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
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerTilÅr tilDatoInfo ->
                    ( { tilDatoInfo | tillatÅViseFeilmeldingÅr = True }
                        |> RegistrerTilÅr
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerRolle rolleInfo ->
                    ( { rolleInfo | tillatÅViseFeilmeldingRolle = True }
                        |> RegistrerRolle
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilEndreOpplysninger ->
            case model.aktivSamtale of
                VisOppsummering validertAnnenErfaringSkjema ->
                    updateEtterVilEndreSkjema model validertAnnenErfaringSkjema

                VisOppsummeringEtterEndring validertAnnenErfaringSkjema ->
                    updateEtterVilEndreSkjema model validertAnnenErfaringSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger annenErfaringSkjema ->
                    ( annenErfaringSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger
                        |> oppdaterSamtaleSteg model
                    , Cmd.none
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
                                |> VisOppsummeringEtterEndring
                                |> nesteSamtaleSteg model (Melding.svar (validertSkjemaTilSetninger validertSkjema))
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( skjema
                                |> Skjema.tillatÅViseAlleFeilmeldinger
                                |> EndreOpplysninger
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreAnnenErfaring ->
            case model.aktivSamtale of
                VisOppsummering skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                VisOppsummeringEtterEndring skjema ->
                    updateEtterLagreKnappTrykket model skjema (Melding.svar [ "Ja, informasjonen er riktig" ])

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSkjema skjema
                        |> nesteSamtaleSteg model (Melding.svar [ "Prøv igjen" ])
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
                            ( annenErfaringer
                                |> VenterPåAnimasjonFørFullføring
                                |> oppdaterSamtaleSteg { model | seksjonsMeldingsLogg = oppdatertMeldingslogg }
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtaleSteg model
                                    , Api.postAnnenErfaring AnnenErfaringLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtaleSteg model
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre annen erfaring" error
                                    )
                                        |> IkkeFerdig

                            else
                                ( skjema
                                    |> LagringFeilet error
                                    |> nesteSamtaleStegUtenMelding model
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
                    ( model.annenErfaringListe
                        |> VenterPåAnimasjonFørFullføring
                        |> nesteSamtaleSteg model (Melding.svar [ "Gå videre" ])
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
                                |> oppdaterSamtaleSteg model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        LagringFeilet error skjema ->
                            if ErrorHåndtering.operasjonEtterError error == LoggInn then
                                IkkeFerdig
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtaleSteg model
                                    , Api.postAnnenErfaring AnnenErfaringLagret skjema
                                    )

                            else
                                IkkeFerdig ( Model model, Cmd.none )

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

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

        ViewportSatt _ ->
            IkkeFerdig ( Model model, Cmd.none )

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


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring annenErfaringListe ->
                    Ferdig annenErfaringListe ferdigAnimertSamtale

                _ ->
                    ( Model
                        { model
                            | seksjonsMeldingsLogg =
                                nyMeldingsLogg
                        }
                    , Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        , settFokus model.aktivSamtale
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


updateEtterVilEndreSkjema : ModelInfo -> ValidertAnnenErfaringSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger
        |> nesteSamtaleSteg model (Melding.svar [ "Nei, jeg vil endre" ])
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> ValidertAnnenErfaringSkjema -> Melding -> SamtaleStatus
updateEtterLagreKnappTrykket model skjema melding =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> nesteSamtaleSteg model melding
    , Api.postAnnenErfaring AnnenErfaringLagret skjema
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


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg modelInfo melding samtale =
    Model
        { modelInfo
            | aktivSamtale = samtale
            , seksjonsMeldingsLogg =
                modelInfo.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
        }


nesteSamtaleStegUtenMelding : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenMelding model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg annenErfaringSeksjon =
    case annenErfaringSeksjon of
        RegistrerRolle _ ->
            [ Melding.spørsmål [ "Så bra at du har mer erfaring. Hvilken rolle har du hatt?" ]
            , Melding.spørsmål
                [ "Har du jobbet som fotballtrener, kanskje besøksvenn eller noe helt annet?" ]
            ]

        RegistrerBeskrivelse _ ->
            [ Melding.spørsmål [ "Hva var jobben din?" ]
            ]

        SpørOmBrukerVilLeggeInnTidsperiode _ ->
            [ Melding.spørsmål [ "Det kan være viktig for arbeidsgiver å vite hvor lenge du har hatt rollen. Har du lyst til å legge inn informasjonen om tidsperiode?" ]
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

        VisOppsummering skjema ->
            [ [ [ "Du har lagt inn dette:"
                , Melding.tomLinje
                ]
              , validertSkjemaTilSetninger skjema
              , [ Melding.tomLinje
                , "Er informasjonen riktig?"
                ]
              ]
                |> List.concat
                |> Melding.spørsmål
            ]

        EndreOpplysninger _ ->
            []

        VisOppsummeringEtterEndring _ ->
            [ Melding.spørsmål [ "Er informasjonen riktig nå?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre annen erfaring" } ]

        VenterPåAnimasjonFørFullføring _ ->
            []


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
    inputId
        |> inputIdTilString
        |> Dom.focus
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
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                RegistrerRolle info ->
                    Containers.inputMedGåVidereKnapp VilRegistrereRolle
                        [ info.rolle
                            |> Input.input { label = "Rolle", msg = OppdatererRolle }
                            |> Input.withOnEnter VilRegistrereRolle
                            |> Input.withId (inputIdTilString RolleId)
                            |> Input.withOnBlur FeltMisterFokus
                            |> Input.withMaybeFeilmelding
                                (info.rolle
                                    |> Skjema.feilmeldingRolle
                                    |> maybeHvisTrue info.tillatÅViseFeilmeldingRolle
                                )
                            |> Input.toHtml
                        ]

                RegistrerBeskrivelse info ->
                    Containers.inputMedGåVidereKnapp VilRegistrereBeskrivelse
                        [ info.beskrivelse
                            |> Textarea.textarea { label = "Beskriv oppgavene dine", msg = OppdatererBeskrivelse }
                            |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn info.beskrivelse maxLengthBeskrivelse)
                            |> Textarea.withId (inputIdTilString BeskrivelseId)
                            |> Textarea.toHtml
                        ]

                SpørOmBrukerVilLeggeInnTidsperiode _ ->
                    Containers.knapper Flytende
                        [ "Ja, det vil jeg"
                            |> Knapp.knapp SvarerJaTilTidsperiode
                            |> Knapp.toHtml
                        , "Nei, det vil jeg ikke"
                            |> Knapp.knapp SvarerNeiTilTidsperiode
                            |> Knapp.toHtml
                        ]

                RegistrerFraMåned _ ->
                    MånedKnapper.månedKnapper FraMånedValgt

                RegistrerFraÅr fraDatoInfo ->
                    Containers.inputMedGåVidereKnapp VilRegistrereFraÅr
                        [ div [ class "år-wrapper" ]
                            [ fraDatoInfo.fraÅr
                                |> Input.input { label = "År", msg = OppdatererFraÅr }
                                |> Input.withClass "aar"
                                |> Input.withOnEnter VilRegistrereFraÅr
                                |> Input.withOnBlur FeltMisterFokus
                                |> Input.withId (inputIdTilString FraÅrId)
                                |> Input.withMaybeFeilmelding
                                    (fraDatoInfo.fraÅr
                                        |> Dato.feilmeldingÅr
                                        |> maybeHvisTrue fraDatoInfo.tillatÅViseFeilmeldingÅr
                                    )
                                |> Input.toHtml
                            ]
                        ]

                RegistrerNåværende _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp SvarerJaTilNåværende "Ja"
                            |> Knapp.toHtml
                        , Knapp.knapp SvarerNeiTilNåværende "Nei"
                            |> Knapp.toHtml
                        ]

                RegistrerTilMåned _ ->
                    MånedKnapper.månedKnapper TilMånedValgt

                RegistrerTilÅr tilDatoInfo ->
                    Containers.inputMedGåVidereKnapp VilRegistrereTilÅr
                        [ div [ class "år-wrapper" ]
                            [ tilDatoInfo.tilÅr
                                |> Input.input { label = "År", msg = OppdatererTilÅr }
                                |> Input.withClass "aar"
                                |> Input.withOnEnter VilRegistrereTilÅr
                                |> Input.withOnBlur FeltMisterFokus
                                |> Input.withId (inputIdTilString TilÅrId)
                                |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue tilDatoInfo.tillatÅViseFeilmeldingÅr) tilDatoInfo.tilÅr)
                                |> Input.toHtml
                            ]
                        ]

                VisOppsummering _ ->
                    viewBekreftOppsummering

                VisOppsummeringEtterEndring _ ->
                    viewBekreftOppsummering

                EndreOpplysninger skjema ->
                    Containers.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                        [ skjema
                            |> Skjema.innholdTekstFelt Rolle
                            |> Input.input { label = "Rolle", msg = Tekst Rolle >> SkjemaEndret }
                            |> Input.withMaybeFeilmelding (Skjema.feilmeldingRolleHvisSynlig skjema)
                            |> Input.withOnBlur (SkjemaEndret RolleBlurred)
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

                LagrerSkjema _ lagreStatus ->
                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                        LoggInnLenke.viewLoggInnLenke

                    else
                        text ""

                LagringFeilet error _ ->
                    case ErrorHåndtering.operasjonEtterError error of
                        ErrorHåndtering.GiOpp ->
                            Containers.knapper Flytende
                                [ Knapp.knapp FerdigMedAnnenErfaring "Gå videre"
                                    |> Knapp.toHtml
                                ]

                        ErrorHåndtering.PrøvPåNytt ->
                            Containers.knapper Flytende
                                [ Knapp.knapp VilLagreAnnenErfaring "Prøv igjen"
                                    |> Knapp.toHtml
                                , Knapp.knapp FerdigMedAnnenErfaring "Gå videre"
                                    |> Knapp.toHtml
                                ]

                        ErrorHåndtering.LoggInn ->
                            LoggInnLenke.viewLoggInnLenke

                VenterPåAnimasjonFørFullføring _ ->
                    text ""

        MeldingerGjenstår ->
            text ""


viewBekreftOppsummering : Html Msg
viewBekreftOppsummering =
    Containers.knapper Flytende
        [ Knapp.knapp VilLagreAnnenErfaring "Ja, informasjonen er riktig"
            |> Knapp.toHtml
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
            |> Knapp.toHtml
        ]


viewDatoPeriode : AnnenErfaringSkjema -> Html Msg
viewDatoPeriode skjema =
    div []
        [ div [ class "DatoInput-fra-til-rad" ]
            [ ValgfriDatoInput.datoInput
                { label = "Fra"
                , onMånedChange = FraMåned >> SkjemaEndret
                , måned = Skjema.fraMåned skjema
                , onÅrChange = Tekst FraÅr >> SkjemaEndret
                , år = Skjema.innholdTekstFelt FraÅr skjema
                }
                |> ValgfriDatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFraÅr skjema)
                |> ValgfriDatoInput.withMaybeFeilmeldingMåned (Skjema.feilmeldingFraMåned skjema)
                |> ValgfriDatoInput.withOnBlurÅr (SkjemaEndret FraÅrBlurred)
                |> ValgfriDatoInput.toHtml
            , if not (Skjema.nåværende skjema) then
                ValgfriDatoInput.datoInput
                    { label = "Til"
                    , onMånedChange = TilMåned >> SkjemaEndret
                    , måned = Skjema.tilMåned skjema
                    , onÅrChange = Tekst TilÅr >> SkjemaEndret
                    , år = Skjema.innholdTekstFelt TilÅr skjema
                    }
                    |> ValgfriDatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingTilÅr skjema)
                    |> ValgfriDatoInput.withMaybeFeilmeldingMåned (Skjema.feilmeldingTilMåned skjema)
                    |> ValgfriDatoInput.withOnBlurÅr (SkjemaEndret TilÅrBlurred)
                    |> ValgfriDatoInput.toHtml

              else
                text ""
            ]
        , skjema
            |> Skjema.nåværende
            |> Checkbox.checkbox "Nåværende" (SkjemaEndret NåværendeToggled)
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
subscriptions model =
    Browser.Events.onVisibilityChange WindowEndrerVisibility
