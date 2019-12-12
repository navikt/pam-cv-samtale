module Sertifikat.Seksjon exposing
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
import Cv.Sertifikat exposing (Sertifikat)
import Dato exposing (Måned(..), År, datoTilString)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.Checkbox as Checkbox
import FrontendModuler.DatoInput as DatoInput
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Typeahead
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import LagreStatus exposing (LagreStatus)
import Meldinger.Melding as Melding exposing (Melding(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Sertifikat.SertifikatTypeahead as SertifikatTypeahead exposing (SertifikatTypeahead)
import Sertifikat.Skjema as Skjema exposing (SertifikatFelt(..), SertifikatSkjema, Utløpsdato(..), ValidertSertifikatSkjema)
import Task
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..), InputStatus(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , sertifikatListe : List Sertifikat
    , debugStatus : DebugStatus
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Sertifikat) FerdigAnimertMeldingsLogg


type AvsluttetGrunn
    = SlettetPåbegynt
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = RegistrerSertifikatFelt Bool (Typeahead.Model SertifikatTypeahead)
    | RegistrerUtsteder UtstederInfo
    | RegistrerFullførtMåned FullførtDatoInfo
    | RegistrerFullførtÅr FullførtDatoInfo
    | SpørOmUtløpsdatoFinnes ValidertFullførtDatoInfo
    | RegistrerUtløperMåned UtløpsdatoInfo
    | RegistrerUtløperÅr UtløpsdatoInfo
    | VisOppsummering OppsummeringsType ValidertSertifikatSkjema
    | EndreOpplysninger (Typeahead.Model SertifikatTypeahead) SertifikatSkjema
    | BekreftSlettingAvPåbegynt ValidertSertifikatSkjema
    | LagrerSkjema ValidertSertifikatSkjema LagreStatus
    | LagringFeilet Http.Error ValidertSertifikatSkjema
    | VenterPåAnimasjonFørFullføring (List Sertifikat) AvsluttetGrunn


type alias UtstederInfo =
    { sertifikatFelt : SertifikatFelt
    , utsteder : String
    }


type alias FullførtDatoInfo =
    { sertifikat : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : String
    , visFeilmeldingFullførtÅr : Bool
    }


type alias ValidertFullførtDatoInfo =
    { sertifikat : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    }


type alias UtløpsdatoInfo =
    { forrigeFeltInfo : ValidertFullførtDatoInfo
    , utløperMåned : Måned
    , utløperÅr : String
    , visFeilmeldingUtløperÅr : Bool
    }


sertifikatFeltTilUtsteder : SertifikatFelt -> UtstederInfo
sertifikatFeltTilUtsteder sertifikatFelt =
    { sertifikatFelt = sertifikatFelt
    , utsteder = ""
    }


utstederTilFullførtDato : UtstederInfo -> FullførtDatoInfo
utstederTilFullførtDato input =
    { sertifikat = input.sertifikatFelt
    , utsteder = input.utsteder
    , fullførtMåned = Januar
    , fullførtÅr = ""
    , visFeilmeldingFullførtÅr = False
    }


validertFullførtDatoTilUtløpsdato : ValidertFullførtDatoInfo -> UtløpsdatoInfo
validertFullførtDatoTilUtløpsdato input =
    { forrigeFeltInfo = input
    , utløperMåned = Januar
    , utløperÅr = ""
    , visFeilmeldingUtløperÅr = False
    }


validertFullførtDatoTilSkjema : ValidertFullførtDatoInfo -> ValidertSertifikatSkjema
validertFullførtDatoTilSkjema input =
    Skjema.initValidertSkjema
        { sertifikatFelt = input.sertifikat
        , utsteder = input.utsteder
        , fullførtMåned = input.fullførtMåned
        , fullførtÅr = input.fullførtÅr
        , utløpsdato = IkkeOppgitt
        , id = Nothing
        }


utløpsdatoTilSkjema : UtløpsdatoInfo -> År -> ValidertSertifikatSkjema
utløpsdatoTilSkjema info år =
    Skjema.initValidertSkjema
        { sertifikatFelt = info.forrigeFeltInfo.sertifikat
        , utsteder = info.forrigeFeltInfo.utsteder
        , fullførtMåned = info.forrigeFeltInfo.fullførtMåned
        , fullførtÅr = info.forrigeFeltInfo.fullførtÅr
        , utløpsdato = Oppgitt info.utløperMåned år
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = TypeaheadMsg (Typeahead.Msg SertifikatTypeahead)
    | HentetTypeahead (Result Http.Error (List SertifikatTypeahead))
    | VilRegistrereSertifikat
    | VilRegistrereUtsteder
    | OppdatererUtsteder String
    | FullførtMånedValgt Dato.Måned
    | VilRegistrereFullførtÅr
    | OppdatererFullførtÅr String
    | VilRegistrereUtløperMåned
    | VilIkkeRegistrereUtløpesdato
    | UtløperMånedValgt Dato.Måned
    | VilRegistrereUtløperÅr
    | OppdatererUtløperÅr String
    | VilLagreSertifikat
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | VilLagreEndretSkjema
    | SertifikatLagret (Result Http.Error (List Sertifikat))
    | FerdigMedSertifikat
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | ÅrMisterFokus
    | ErrorLogget


type SkjemaEndring
    = Utsteder String
    | FullførtMåned String
    | FullførtÅr String
    | FullførtÅrMistetFokus
    | UtløperIkkeToggled
    | UtløperMåned String
    | UtløperÅr String
    | UtløperÅrMistetFokus


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        TypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt visFeilmelding typeaheadModel ->
                    updateSamtaleTypeahead model visFeilmelding typeaheadMsg typeaheadModel

                EndreOpplysninger typeaheadModel skjema ->
                    let
                        ( nyTypeaheadModel, status ) =
                            Typeahead.update SertifikatTypeahead.label typeaheadMsg typeaheadModel
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> tilSertifikatFelt
                            |> Skjema.oppdaterSertifikat skjema
                            |> Skjema.visFeilmeldingSertifikatFelt (Typeahead.inputStatus status == InputBlurred)
                            |> EndreOpplysninger nyTypeaheadModel
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , case Typeahead.getSuggestionsStatus status of
                            GetSuggestionsForInput query ->
                                Api.getSertifikatTypeahead HentetTypeahead query

                            DoNothing ->
                                Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead result ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt visFeilmelding typeaheadModel ->
                    case result of
                        Ok suggestions ->
                            let
                                nyTypeaheadModel =
                                    Typeahead.updateSuggestions SertifikatTypeahead.label typeaheadModel suggestions
                            in
                            ( nyTypeaheadModel
                                |> RegistrerSertifikatFelt visFeilmelding
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hent SertifikatTypeahead" )
                                |> IkkeFerdig

                EndreOpplysninger typeaheadModel skjema ->
                    case result of
                        Ok suggestions ->
                            ( EndreOpplysninger (Typeahead.updateSuggestions SertifikatTypeahead.label typeaheadModel suggestions) skjema
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( Model model, logFeilmelding error "Hent SertifikatTypeahead" )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereSertifikat ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt _ typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just sertifikat ->
                            sertifikat
                                |> SertifikatFraTypeahead
                                |> brukerVelgerSertifikatFelt model

                        Nothing ->
                            brukerVilRegistrereFritekstSertifikat model typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereUtsteder ->
            case model.aktivSamtale of
                RegistrerUtsteder input ->
                    ( input
                        |> utstederTilFullførtDato
                        |> RegistrerFullførtMåned
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OppdatererUtsteder string ->
            case model.aktivSamtale of
                RegistrerUtsteder utsteder ->
                    ( { utsteder | utsteder = string }
                        |> RegistrerUtsteder
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        FullførtMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerFullførtMåned fullførtDatoInfo ->
                    ( måned
                        |> setFullførtMåned fullførtDatoInfo
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereFullførtÅr ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    case Dato.stringTilÅr fullførtDatoInfo.fullførtÅr of
                        Just fullførtÅr ->
                            ( { sertifikat = fullførtDatoInfo.sertifikat
                              , utsteder = fullførtDatoInfo.utsteder
                              , fullførtMåned = fullførtDatoInfo.fullførtMåned
                              , fullførtÅr = fullførtÅr
                              }
                                |> SpørOmUtløpsdatoFinnes
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { fullførtDatoInfo | visFeilmeldingFullførtÅr = True }
                                |> RegistrerFullførtÅr
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererFullførtÅr string ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | fullførtÅr = string }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilIkkeRegistrereUtløpesdato ->
            case model.aktivSamtale of
                SpørOmUtløpsdatoFinnes fullførtDatoInfo ->
                    ( fullførtDatoInfo
                        |> validertFullførtDatoTilSkjema
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereUtløperMåned ->
            case model.aktivSamtale of
                SpørOmUtløpsdatoFinnes fullførtDatoInfo ->
                    ( fullførtDatoInfo
                        |> validertFullførtDatoTilUtløpsdato
                        |> RegistrerUtløperMåned
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        UtløperMånedValgt måned ->
            case model.aktivSamtale of
                RegistrerUtløperMåned utløpsdatoInfo ->
                    ( { utløpsdatoInfo | utløperMåned = måned }
                        |> RegistrerUtløperÅr
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereUtløperÅr ->
            case model.aktivSamtale of
                RegistrerUtløperÅr utløpsdatoInfo ->
                    case Dato.stringTilÅr utløpsdatoInfo.utløperÅr of
                        Just utløperÅr ->
                            ( utløpsdatoTilSkjema utløpsdatoInfo utløperÅr
                                |> VisOppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { utløpsdatoInfo | visFeilmeldingUtløperÅr = True }
                                |> RegistrerUtløperÅr
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdatererUtløperÅr string ->
            case model.aktivSamtale of
                RegistrerUtløperÅr utløpsdatoInfo ->
                    ( { utløpsdatoInfo | utløperÅr = string }
                        |> RegistrerUtløperÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        ÅrMisterFokus ->
            case model.aktivSamtale of
                RegistrerFullførtÅr fullførtDatoInfo ->
                    ( { fullførtDatoInfo | visFeilmeldingFullførtÅr = True }
                        |> RegistrerFullførtÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerUtløperÅr utløpsdatoInfo ->
                    ( { utløpsdatoInfo | visFeilmeldingUtløperÅr = True }
                        |> RegistrerUtløperÅr
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilEndreOpplysninger ->
            case model.aktivSamtale of
                VisOppsummering _ validertSertifikatSkjema ->
                    updateEtterVilEndreSkjema model msg validertSertifikatSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger typeaheadModel sertifikatSkjema ->
                    ( sertifikatSkjema
                        |> oppdaterSkjema skjemaEndring
                        |> EndreOpplysninger typeaheadModel
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

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
                    ( VenterPåAnimasjonFørFullføring model.sertifikatListe SlettetPåbegynt
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
                EndreOpplysninger typeaheadModel skjema ->
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
                                |> Skjema.visAlleFeilmeldinger
                                |> EndreOpplysninger typeaheadModel
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilLagreSertifikat ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , postEllerPutSertifikat SertifikatLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SertifikatLagret result ->
            case model.aktivSamtale of
                LagrerSkjema skjema lagreStatus ->
                    case result of
                        Ok sertifikater ->
                            let
                                oppdatertMeldingslogg =
                                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Nå er sertifiseringen lagret 👍" ] ]

                                    else
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Nå er sertifiseringen lagret 👍" ] ]
                            in
                            ( VenterPåAnimasjonFørFullføring sertifikater AnnenAvslutning
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
                                    , postEllerPutSertifikat SertifikatLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre sertifikat" error
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
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre sertifikat" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        FerdigMedSertifikat ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( VenterPåAnimasjonFørFullføring model.sertifikatListe AnnenAvslutning
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

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
                                    , postEllerPutSertifikat SertifikatLagret skjema
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


initSamtaleTypeahead : Typeahead.Model SertifikatTypeahead
initSamtaleTypeahead =
    Typeahead.init
        { value = ""
        , label = "Sertifisering eller sertifikat"
        , id = inputIdTilString SertifikatTypeaheadId
        , toString = SertifikatTypeahead.label
        }


initSkjemaTypeahead : ValidertSertifikatSkjema -> Typeahead.Model SertifikatTypeahead
initSkjemaTypeahead skjema =
    case Skjema.sertifikatFeltValidert skjema of
        SertifikatFraTypeahead sertifikatTypeahead ->
            Typeahead.initWithSelected
                { selected = sertifikatTypeahead
                , label = "Sertifisering eller sertifikat"
                , id = inputIdTilString SertifikatTypeaheadId
                , toString = SertifikatTypeahead.label
                }

        Egendefinert inputValue ->
            Typeahead.init
                { value = inputValue
                , label = "Sertifisering eller sertifikat"
                , id = inputIdTilString SertifikatTypeaheadId
                , toString = SertifikatTypeahead.label
                }


sertifikatFeltFraTypeaheadModel : Typeahead.Model SertifikatTypeahead -> Maybe SertifikatFelt
sertifikatFeltFraTypeaheadModel typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just sertifikat ->
            Just (SertifikatFraTypeahead sertifikat)

        Nothing ->
            let
                trimmedInputString =
                    typeaheadModel
                        |> Typeahead.inputValue
                        |> String.trim
            in
            if String.length trimmedInputString > 0 then
                typeaheadModel
                    |> Typeahead.inputValue
                    |> Egendefinert
                    |> Just

            else
                Nothing


brukerVilRegistrereFritekstSertifikat : ModelInfo -> Typeahead.Model SertifikatTypeahead -> SamtaleStatus
brukerVilRegistrereFritekstSertifikat model typeaheadModel =
    case sertifikatFeltFraTypeaheadModel typeaheadModel of
        Just sertifikatFelt ->
            brukerVelgerSertifikatFelt model sertifikatFelt

        Nothing ->
            visFeilmeldingRegistrerSertifikat model typeaheadModel


visFeilmeldingRegistrerSertifikat : ModelInfo -> Typeahead.Model SertifikatTypeahead -> SamtaleStatus
visFeilmeldingRegistrerSertifikat model typeaheadModel =
    ( typeaheadModel
        |> RegistrerSertifikatFelt True
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


tilSertifikatFelt : Typeahead.Model SertifikatTypeahead -> SertifikatFelt
tilSertifikatFelt typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just sertifikatTypeahead ->
            SertifikatFraTypeahead sertifikatTypeahead

        Nothing ->
            Egendefinert (Typeahead.inputValue typeaheadModel)


updateSamtaleTypeahead : ModelInfo -> Bool -> Typeahead.Msg SertifikatTypeahead -> Typeahead.Model SertifikatTypeahead -> SamtaleStatus
updateSamtaleTypeahead model visFeilmelding msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update SertifikatTypeahead.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case sertifikatFeltFraTypeaheadModel nyTypeaheadModel of
                Just sertifikatFelt ->
                    brukerVelgerSertifikatFelt model sertifikatFelt

                Nothing ->
                    visFeilmeldingRegistrerSertifikat model nyTypeaheadModel

        Typeahead.InputBlurred ->
            visFeilmeldingRegistrerSertifikat model nyTypeaheadModel

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerSertifikatFelt visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput string ->
                        Api.getSertifikatTypeahead HentetTypeahead string

                    DoNothing ->
                        Cmd.none
                )


feilmeldingTypeahead : Typeahead.Model SertifikatTypeahead -> Maybe String
feilmeldingTypeahead typeaheadModel =
    case sertifikatFeltFraTypeaheadModel typeaheadModel of
        Just _ ->
            Nothing

        Nothing ->
            Just "Velg eller skriv inn sertifisering eller sertifikat"


setFullførtMåned : FullførtDatoInfo -> Dato.Måned -> FullførtDatoInfo
setFullførtMåned fullførtDatoInfo måned =
    { fullførtDatoInfo | fullførtMåned = måned }


oppdaterSkjema : SkjemaEndring -> SertifikatSkjema -> SertifikatSkjema
oppdaterSkjema endring skjema =
    case endring of
        Utsteder string ->
            Skjema.oppdaterUtsteder skjema string

        FullførtMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> Skjema.oppdaterFullførtMåned skjema

        FullførtÅr string ->
            Skjema.oppdaterFullførtÅr skjema string

        UtløperMåned månedString ->
            månedString
                |> Dato.stringTilMåned
                |> Skjema.oppdaterUtløperMåned skjema

        UtløperÅr string ->
            Skjema.oppdaterUtløperÅr skjema string

        UtløperIkkeToggled ->
            Skjema.toggleUtløperIkke skjema

        FullførtÅrMistetFokus ->
            Skjema.visFeilmeldingFullførtÅr skjema

        UtløperÅrMistetFokus ->
            Skjema.visFeilmeldingUtløperÅr skjema


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring sertifikatListe _ ->
                    Ferdig sertifikatListe ferdigAnimertSamtale

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


brukerVelgerSertifikatFelt : ModelInfo -> SertifikatFelt -> SamtaleStatus
brukerVelgerSertifikatFelt info sertifikatFelt =
    ( sertifikatFelt
        |> sertifikatFeltTilUtsteder
        |> RegistrerUtsteder
        |> oppdaterSamtale info (ManueltSvar (Melding.svar [ sertifikatFeltTilString sertifikatFelt ]))
    , lagtTilSpørsmålCmd info.debugStatus
    )
        |> IkkeFerdig


sertifikatFeltTilString : SertifikatFelt -> String
sertifikatFeltTilString sertifikatFelt =
    case sertifikatFelt of
        SertifikatFraTypeahead sertifikatTypeahead ->
            SertifikatTypeahead.label sertifikatTypeahead

        Egendefinert inputValue ->
            inputValue


updateEtterVilEndreSkjema : ModelInfo -> Msg -> ValidertSertifikatSkjema -> SamtaleStatus
updateEtterVilEndreSkjema model msg skjema =
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger (initSkjemaTypeahead skjema)
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , skjema
            |> Skjema.sertifikatString
            |> Api.getSertifikatTypeahead HentetTypeahead
        ]
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertSertifikatSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , postEllerPutSertifikat SertifikatLagret skjema
    )
        |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError (always ErrorLogget))
        |> Maybe.withDefault Cmd.none


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
samtaleTilMeldingsLogg sertifikatSeksjon =
    case sertifikatSeksjon of
        RegistrerSertifikatFelt _ _ ->
            [ Melding.spørsmål [ "Hva slags sertifikat eller sertifisering har du?" ]
            , Melding.spørsmål [ "Kanskje du har truckførerbevis T1, eller noe helt annet? 😊" ]
            ]

        RegistrerUtsteder _ ->
            [ Melding.spørsmål [ "Hvilken organisasjon sertifiserte deg?" ]
            , Melding.spørsmål [ "Er du usikker på hvem som har ansvar for sertifiseringen? Det står ofte på beviset ditt." ]
            ]

        RegistrerFullførtMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned fullførte du sertifiseringen?" ]
            ]

        RegistrerFullførtÅr _ ->
            [ Melding.spørsmål [ "Hvilket år fullførte du sertifiseringen?" ]
            ]

        SpørOmUtløpsdatoFinnes _ ->
            [ Melding.spørsmål [ "Har sertifiseringen en utløpsdato?" ]
            ]

        RegistrerUtløperMåned _ ->
            [ Melding.spørsmål [ "Hvilken måned utløper sertifiseringen din?" ]
            ]

        RegistrerUtløperÅr _ ->
            [ Melding.spørsmål [ "Hvilket år utløper du sertifiseringen din?" ]
            ]

        VisOppsummering oppsummeringsType skjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Da sletter jeg ikke sertifiseringen/sertifikatet." ]
                    , oppsummeringsSpørsmål skjema
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål skjema ]

        EndreOpplysninger _ _ ->
            [ Melding.spørsmål [ "Endre informasjonen i feltene under." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne sertifiseringen?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre sertifikat/sertifisering" } ]

        VenterPåAnimasjonFørFullføring _ avsluttetGrunn ->
            case avsluttetGrunn of
                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg slettet sertifiseringen/sertifikatet. Vil du legge inn flere kategorier?" ] ]

                _ ->
                    [ Melding.spørsmål [ "Vil du legge inn flere kategorier?" ] ]


validertSkjemaTilSetninger : ValidertSertifikatSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ "Sertifisering/sertifikat: " ++ (validertSkjema |> Skjema.sertifikatString)
    , "Utsteder: " ++ Skjema.utsteder skjema
    , "Fullført: " ++ Dato.datoTilString (Skjema.fullførtMåned skjema) (Skjema.fullførtÅrValidert validertSkjema)
    , "Utløper: " ++ utløpsdatoTilString (Skjema.utløpsdatoValidert validertSkjema)
    ]


oppsummeringsSpørsmål : ValidertSertifikatSkjema -> Melding
oppsummeringsSpørsmål skjema =
    Melding.spørsmål
        ([ "Du har lagt inn dette:"
         , Melding.tomLinje
         ]
            ++ (validertSkjemaTilSetninger skjema
                    ++ [ Melding.tomLinje
                       , "Er informasjonen riktig?"
                       ]
               )
        )


utløpsdatoTilString : Utløpsdato -> String
utløpsdatoTilString utløpsdato =
    case utløpsdato of
        Oppgitt måned_ år_ ->
            datoTilString måned_ år_

        IkkeOppgitt ->
            ""


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerSertifikatFelt _ _ ->
            settFokusCmd SertifikatTypeaheadId

        RegistrerUtsteder _ ->
            settFokusCmd UtstederId

        RegistrerFullførtÅr _ ->
            settFokusCmd FullførtÅrId

        RegistrerUtløperÅr _ ->
            settFokusCmd UtløperÅrId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd Msg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt



--- VIEW ---


type InputId
    = SertifikatTypeaheadId
    | UtstederId
    | FullførtÅrId
    | UtløperÅrId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        UtstederId ->
            "sertifikat-utsteder-id"

        FullførtÅrId ->
            "sertifikat-fullførtår-id"

        SertifikatTypeaheadId ->
            "sertifikat-typeahead-id"

        UtløperÅrId ->
            "sertifikat-utløperår-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            RegistrerSertifikatFelt visFeilmelding typeaheadModel ->
                BrukerInput.typeaheadMedGåVidereKnapp VilRegistrereSertifikat
                    (typeaheadModel
                        |> feilmeldingTypeahead
                        |> maybeHvisTrue visFeilmelding
                        |> Typeahead.toViewElement SertifikatTypeahead.label typeaheadModel
                        |> FrontendModuler.Typeahead.map TypeaheadMsg
                    )

            RegistrerUtsteder input ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereUtsteder
                    (input.utsteder
                        |> Input.input { label = "Utsteder", msg = OppdatererUtsteder }
                        |> Input.withOnEnter VilRegistrereUtsteder
                        |> Input.withId (inputIdTilString UtstederId)
                    )

            RegistrerFullførtMåned _ ->
                BrukerInput.månedKnapper FullførtMånedValgt

            RegistrerFullførtÅr fullførtDatoInfo ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereFullførtÅr
                    (fullførtDatoInfo.fullførtÅr
                        |> Input.input { label = "År", msg = OppdatererFullførtÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereFullførtÅr
                        |> Input.withOnBlur ÅrMisterFokus
                        |> Input.withId (inputIdTilString FullførtÅrId)
                        |> Input.withMaybeFeilmelding
                            (fullførtDatoInfo.fullførtÅr
                                |> Dato.feilmeldingÅr
                                |> maybeHvisTrue fullførtDatoInfo.visFeilmeldingFullførtÅr
                            )
                        |> Input.withErObligatorisk
                    )

            SpørOmUtløpsdatoFinnes _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilRegistrereUtløperMåned "Ja, sertifiseringen utløper"
                    , Knapp.knapp VilIkkeRegistrereUtløpesdato "Nei, sertifiseringen utløper ikke"
                    ]

            RegistrerUtløperMåned _ ->
                BrukerInput.månedKnapper UtløperMånedValgt

            RegistrerUtløperÅr utløpsdatoInfo ->
                BrukerInput.inputMedGåVidereKnapp VilRegistrereUtløperÅr
                    (utløpsdatoInfo.utløperÅr
                        |> Input.input { label = "År", msg = OppdatererUtløperÅr }
                        |> Input.withClass "aar"
                        |> Input.withWrapperClass "år-wrapper"
                        |> Input.withOnEnter VilRegistrereUtløperÅr
                        |> Input.withOnBlur ÅrMisterFokus
                        |> Input.withId (inputIdTilString UtløperÅrId)
                        |> Input.withMaybeFeilmelding ((Dato.feilmeldingÅr >> maybeHvisTrue utløpsdatoInfo.visFeilmeldingUtløperÅr) utløpsdatoInfo.utløperÅr)
                        |> Input.withErObligatorisk
                    )

            VisOppsummering _ _ ->
                viewBekreftOppsummering

            EndreOpplysninger typeaheadModel skjema ->
                BrukerInput.skjema { lagreMsg = VilLagreEndretSkjema, lagreKnappTekst = "Lagre endringer" }
                    [ skjema
                        |> Skjema.feilmeldingSertifikatFelt
                        |> Typeahead.view SertifikatTypeahead.label typeaheadModel
                        |> Html.map TypeaheadMsg
                    , skjema
                        |> Skjema.utsteder
                        |> Input.input { label = "Utsteder", msg = Utsteder >> SkjemaEndret }
                        |> Input.toHtml
                    , div [ class "DatoInput-fra-til-rad" ]
                        [ DatoInput.datoInput
                            { label = "Når fullførte du sertifiseringen?"
                            , onMånedChange = FullførtMåned >> SkjemaEndret
                            , måned = Skjema.fullførtMåned skjema
                            , onÅrChange = FullførtÅr >> SkjemaEndret
                            , år = Skjema.fullførtÅr skjema
                            }
                            |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingFullførtÅr skjema)
                            |> DatoInput.withOnBlurÅr (SkjemaEndret FullførtÅrMistetFokus)
                            |> DatoInput.toHtml
                        , if not (Skjema.utløperIkke skjema) then
                            DatoInput.datoInput
                                { label = "Når utløper sertifiseringen?"
                                , onMånedChange = UtløperMåned >> SkjemaEndret
                                , måned = Skjema.utløperMåned skjema
                                , onÅrChange = UtløperÅr >> SkjemaEndret
                                , år = Skjema.utløperÅr skjema
                                }
                                |> DatoInput.withMaybeFeilmeldingÅr (Skjema.feilmeldingUtløperÅr skjema)
                                |> DatoInput.withOnBlurÅr (SkjemaEndret UtløperÅrMistetFokus)
                                |> DatoInput.toHtml

                          else
                            text ""
                        ]
                    , skjema
                        |> Skjema.utløperIkke
                        |> Checkbox.checkbox "Sertifiseringen utløper ikke" (SkjemaEndret UtløperIkkeToggled)
                        |> Checkbox.withClass "blokk-m"
                        |> Checkbox.toHtml
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
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedSertifikat "Gå videre"
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreSertifikat "Prøv igjen"
                            , Knapp.knapp FerdigMedSertifikat "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

    else
        BrukerInput.utenInnhold


viewBekreftOppsummering : BrukerInput Msg
viewBekreftOppsummering =
    BrukerInput.knapper Kolonne
        [ Knapp.knapp VilLagreSertifikat "Ja, det er riktig"
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
        , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
        ]


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


postEllerPutSertifikat : (Result Error (List Sertifikat) -> msg) -> Skjema.ValidertSertifikatSkjema -> Cmd msg
postEllerPutSertifikat msgConstructor skjema =
    case Skjema.id skjema of
        Just id ->
            Api.putSertifikat msgConstructor skjema id

        Nothing ->
            Api.postSertifikat msgConstructor skjema



--- INIT ---


init : DebugStatus -> FerdigAnimertMeldingsLogg -> List Sertifikat -> ( Model, Cmd Msg )
init debugStatus gammelMeldingsLogg sertifikatListe =
    let
        aktivSamtale =
            RegistrerSertifikatFelt False initSamtaleTypeahead
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , sertifikatListe = sertifikatListe
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
