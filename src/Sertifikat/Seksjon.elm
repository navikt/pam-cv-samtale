module Sertifikat.Seksjon exposing
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

import Api
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Dato.Dato as Dato exposing (År, datoTilString)
import Dato.Maned as Måned exposing (Måned(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
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
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Result.Extra as Result
import Sertifikat.Sertifikat as Sertifikat exposing (Sertifikat)
import Sertifikat.SertifikatTypeahead as SertifikatTypeahead exposing (SertifikatTypeahead)
import Sertifikat.Skjema as Skjema exposing (SertifikatFelt(..), SertifikatSkjema, Utløpsdato(..), ValidertSertifikatSkjema)
import Task
import Tid exposing (nyesteSistLagretVerdi)
import Time exposing (Posix)
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..), InputStatus(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , sertifikatListe : List Sertifikat
    , debugStatus : DebugStatus
    , sistLagretFraForrigeSeksjon : Posix
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


sistLagret : Model -> Posix
sistLagret (Model model) =
    model.sertifikatListe
        |> List.map Sertifikat.sistEndretDato
        |> nyesteSistLagretVerdi model.sistLagretFraForrigeSeksjon


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix (List Sertifikat) FerdigAnimertMeldingsLogg


type AvsluttetGrunn
    = AvbruttPåbegynt
    | SlettetPåbegynt
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = RegistrerSertifikatFelt Bool (Typeahead.Model SertifikatTypeahead)
    | RegistrerUtsteder UtstederInfo
    | RegistrerFullførtDato FullførtDatoInfo
    | SpørOmUtløpsdatoFinnes SpørOmUtløpsdatoInfo
    | RegistrerUtløpsdato UtløpsdatoInfo
    | VisOppsummering OppsummeringsType ValidertSertifikatSkjema
    | EndreOpplysninger (Typeahead.Model SertifikatTypeahead) SertifikatSkjema
    | BekreftSlettingAvPåbegynt ValidertSertifikatSkjema
    | LagrerSkjema ValidertSertifikatSkjema LagreStatus
    | LagringFeilet Http.Error ValidertSertifikatSkjema
    | BekreftAvbrytingAvRegistreringen Samtale
    | VenterPåAnimasjonFørFullføring (List Sertifikat) AvsluttetGrunn


type alias UtstederInfo =
    { sertifikat : SertifikatFelt
    , utsteder : String
    }


type alias FullførtDatoInfo =
    { sertifikat : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : String
    , visFeilmeldingFullførtÅr : Bool
    }


type alias SpørOmUtløpsdatoInfo =
    { sertifikat : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    }


type alias UtløpsdatoInfo =
    { sertifikat : SertifikatFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    , utløperMåned : Måned
    , utløperÅr : String
    , visFeilmeldingUtløperÅr : Bool
    }


sertifikatFeltTilUtsteder : SertifikatFelt -> UtstederInfo
sertifikatFeltTilUtsteder sertifikatFelt =
    { sertifikat = sertifikatFelt
    , utsteder = ""
    }


utstederTilFullførtDato : UtstederInfo -> FullførtDatoInfo
utstederTilFullførtDato input =
    { sertifikat = input.sertifikat
    , utsteder = input.utsteder
    , fullførtÅr = ""
    , fullførtMåned = Januar
    , visFeilmeldingFullførtÅr = False
    }


fullførtDatoTilSpørOmUtløpsdato : FullførtDatoInfo -> År -> SpørOmUtløpsdatoInfo
fullførtDatoTilSpørOmUtløpsdato input år =
    { sertifikat = input.sertifikat
    , utsteder = input.utsteder
    , fullførtMåned = input.fullførtMåned
    , fullførtÅr = år
    }


spørOmUtløpsdatoInfoTilUtløpsdato : SpørOmUtløpsdatoInfo -> UtløpsdatoInfo
spørOmUtløpsdatoInfoTilUtløpsdato input =
    { sertifikat = input.sertifikat
    , utsteder = input.utsteder
    , fullførtÅr = input.fullførtÅr
    , fullførtMåned = input.fullførtMåned
    , utløperMåned = Januar
    , utløperÅr = ""
    , visFeilmeldingUtløperÅr = False
    }


spørOmUtløpsdatoInfoTilSkjema : SpørOmUtløpsdatoInfo -> ValidertSertifikatSkjema
spørOmUtløpsdatoInfoTilSkjema input =
    Skjema.initValidertSkjema
        { sertifikatFelt = input.sertifikat
        , utsteder = input.utsteder
        , fullførtMåned = input.fullførtMåned
        , fullførtÅr = input.fullførtÅr
        , utløpsdato = IkkeOppgitt
        , id = Nothing
        }


utløpsdatoInfoTilSkjema : UtløpsdatoInfo -> År -> ValidertSertifikatSkjema
utløpsdatoInfoTilSkjema info år =
    Skjema.initValidertSkjema
        { sertifikatFelt = info.sertifikat
        , utsteder = info.utsteder
        , fullførtMåned = info.fullførtMåned
        , fullførtÅr = info.fullførtÅr
        , utløpsdato = Oppgitt info.utløperMåned år
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = TypeaheadMsg (Typeahead.Msg SertifikatTypeahead)
    | HentetTypeahead Typeahead.Query (Result Http.Error (List SertifikatTypeahead))
    | VilRegistrereSertifikat
    | OppdatererUtsteder String
    | VilRegistrereUtsteder
    | OppdatererFullførtMåned String
    | OppdatererFullførtÅr String
    | VilRegistrereFullførtDato
    | SvarerJaTilUtløpsdato
    | SvarerNeiTilUtløpsdato
    | OppdatererUtløperMåned String
    | OppdatererUtløperÅr String
    | VilRegistrereUtløpsdato
    | VilLagreSertifikat
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | VilLagreEndretSkjema
    | SertifikatLagret (Result Http.Error (List Sertifikat))
    | VilAvbryteRegistreringen
    | BekrefterAvbrytingAvRegistrering
    | VilIkkeAvbryteRegistreringen
    | FerdigMedSertifikat
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | FokusSatt (Result Dom.Error ())
    | ÅrMisterFokus
    | TimeoutEtterAtFeltMistetFokus
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

        HentetTypeahead query result ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt visFeilmelding typeaheadModel ->
                    ( result
                        |> Typeahead.updateSuggestions SertifikatTypeahead.label typeaheadModel query
                        |> RegistrerSertifikatFelt visFeilmelding
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hent SertifikatTypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOpplysninger typeaheadModel skjema ->
                    ( EndreOpplysninger (Typeahead.updateSuggestions SertifikatTypeahead.label typeaheadModel query result) skjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hent SertifikatTypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
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
                        |> RegistrerFullførtDato
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

        OppdatererFullførtMåned string ->
            case model.aktivSamtale of
                RegistrerFullførtDato fullførtDatoInfo ->
                    ( { fullførtDatoInfo | fullførtMåned = Måned.stringTilMåned string }
                        |> RegistrerFullførtDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OppdatererFullførtÅr string ->
            case model.aktivSamtale of
                RegistrerFullførtDato fullførtDatoInfo ->
                    ( { fullførtDatoInfo | fullførtÅr = string }
                        |> RegistrerFullførtDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereFullførtDato ->
            case model.aktivSamtale of
                RegistrerFullførtDato info ->
                    case Dato.stringTilÅr info.fullførtÅr of
                        Just fullførtÅr ->
                            ( fullførtÅr
                                |> fullførtDatoTilSpørOmUtløpsdato info
                                |> SpørOmUtløpsdatoFinnes
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { info | visFeilmeldingFullførtÅr = True }
                                |> RegistrerFullførtDato
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SvarerNeiTilUtløpsdato ->
            case model.aktivSamtale of
                SpørOmUtløpsdatoFinnes fullførtDatoInfo ->
                    ( fullførtDatoInfo
                        |> spørOmUtløpsdatoInfoTilSkjema
                        |> VisOppsummering FørsteGang
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        SvarerJaTilUtløpsdato ->
            case model.aktivSamtale of
                SpørOmUtløpsdatoFinnes fullførtDatoInfo ->
                    ( fullførtDatoInfo
                        |> spørOmUtløpsdatoInfoTilUtløpsdato
                        |> RegistrerUtløpsdato
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OppdatererUtløperMåned string ->
            case model.aktivSamtale of
                RegistrerUtløpsdato utløpsdatoInfo ->
                    ( { utløpsdatoInfo | utløperMåned = Måned.stringTilMåned string }
                        |> RegistrerUtløpsdato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        OppdatererUtløperÅr string ->
            case model.aktivSamtale of
                RegistrerUtløpsdato utløpsdatoInfo ->
                    ( { utløpsdatoInfo | utløperÅr = string }
                        |> RegistrerUtløpsdato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilRegistrereUtløpsdato ->
            case model.aktivSamtale of
                RegistrerUtløpsdato info ->
                    case Dato.stringTilÅr info.utløperÅr of
                        Just utløperÅr ->
                            ( utløperÅr
                                |> utløpsdatoInfoTilSkjema info
                                |> VisOppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( { info | visFeilmeldingUtløperÅr = True }
                                |> RegistrerUtløpsdato
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        ÅrMisterFokus ->
            IkkeFerdig ( Model model, mistetFokusCmd )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt _ typeaheadModel ->
                    visFeilmeldingRegistrerSertifikat model typeaheadModel

                RegistrerFullførtDato fullførtDatoInfo ->
                    ( { fullførtDatoInfo | visFeilmeldingFullførtÅr = True }
                        |> RegistrerFullførtDato
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                RegistrerUtløpsdato utløpsdatoInfo ->
                    ( { utløpsdatoInfo | visFeilmeldingUtløperÅr = True }
                        |> RegistrerUtløpsdato
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
                    , lagreSertifikat SertifikatLagret skjema
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
                                |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg, sertifikatListe = sertifikater } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , lagreSertifikat SertifikatLagret skjema
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

        VilAvbryteRegistreringen ->
            case model.aktivSamtale of
                RegistrerSertifikatFelt _ _ ->
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
                                    , lagreSertifikat SertifikatLagret skjema
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
            case model.aktivSamtale of
                EndreOpplysninger typeaheadModel skjema ->
                    IkkeFerdig
                        ( EndreOpplysninger (Typeahead.hideSuggestions typeaheadModel) skjema
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget ->
            IkkeFerdig ( Model model, Cmd.none )


initSamtaleTypeahead : ( Typeahead.Model SertifikatTypeahead, Typeahead.Query )
initSamtaleTypeahead =
    Typeahead.init
        { value = ""
        , label = "Sertifisering eller sertifikat"
        , id = inputIdTilString SertifikatTypeaheadId
        , toString = SertifikatTypeahead.label
        }


initSkjemaTypeahead : ValidertSertifikatSkjema -> ( Typeahead.Model SertifikatTypeahead, Typeahead.Query )
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


avbrytRegistrering : ModelInfo -> Msg -> SamtaleStatus
avbrytRegistrering model msg =
    ( AvbruttPåbegynt
        |> VenterPåAnimasjonFørFullføring model.sertifikatListe
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


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
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerSertifikatFelt visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

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

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerSertifikatFelt visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView SertifikatTypeahead.label Nothing
                    |> Cmd.map TypeaheadMsg
                )


feilmeldingTypeahead : Typeahead.Model SertifikatTypeahead -> Maybe String
feilmeldingTypeahead typeaheadModel =
    case sertifikatFeltFraTypeaheadModel typeaheadModel of
        Just _ ->
            Nothing

        Nothing ->
            Just "Velg eller skriv inn sertifisering eller sertifikat"


oppdaterSkjema : SkjemaEndring -> SertifikatSkjema -> SertifikatSkjema
oppdaterSkjema endring skjema =
    case endring of
        Utsteder string ->
            Skjema.oppdaterUtsteder skjema string

        FullførtMåned månedString ->
            månedString
                |> Måned.stringTilMåned
                |> Skjema.oppdaterFullførtMåned skjema

        FullførtÅr string ->
            Skjema.oppdaterFullførtÅr skjema string

        UtløperMåned månedString ->
            månedString
                |> Måned.stringTilMåned
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
                    Ferdig (sistLagret (Model model)) sertifikatListe ferdigAnimertSamtale

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
    let
        ( typeaheadModel, query ) =
            initSkjemaTypeahead skjema
    in
    ( skjema
        |> Skjema.tilUvalidertSkjema
        |> EndreOpplysninger typeaheadModel
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , Api.getSertifikatTypeahead HentetTypeahead query
        ]
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertSertifikatSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagreSertifikat SertifikatLagret skjema
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


logFeilmelding : String -> Http.Error -> Cmd Msg
logFeilmelding operasjon error =
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

        RegistrerFullførtDato _ ->
            [ Melding.spørsmål [ "Når fullførte du sertifiseringen?" ]
            ]

        SpørOmUtløpsdatoFinnes _ ->
            [ Melding.spørsmål [ "Har sertifiseringen en utløpsdato?" ]
            ]

        RegistrerUtløpsdato _ ->
            [ Melding.spørsmål [ "Når utløper sertifiseringen din?" ]
            ]

        VisOppsummering oppsummeringsType skjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Ok, da lar jeg sertifiseringen/sertifikatet stå." ]
                    , oppsummeringsSpørsmål skjema
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål skjema ]

        EndreOpplysninger _ _ ->
            [ Melding.spørsmål [ "Endre informasjonen i feltene under." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne sertifiseringen/sertifikatet?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre sertifikat/sertifisering" } ]

        BekreftAvbrytingAvRegistreringen _ ->
            [ Melding.spørsmål [ "Hvis du avbryter, blir ikke sertifikatet/sertifiseringen lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

        VenterPåAnimasjonFørFullføring _ avsluttetGrunn ->
            case avsluttetGrunn of
                AvbruttPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg avbrutt. Vil du legge inn flere kategorier?" ] ]

                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg slettet sertifiseringen/sertifikatet. Vil du legge inn flere kategorier?" ] ]

                AnnenAvslutning ->
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

        RegistrerFullførtDato _ ->
            settFokusCmd FullførtMånedId

        SpørOmUtløpsdatoFinnes _ ->
            settFokusCmd LeggTilUtløperId

        RegistrerUtløpsdato _ ->
            settFokusCmd UtløperMånedId

        VisOppsummering _ _ ->
            settFokusCmd BekreftOppsummeringId

        EndreOpplysninger _ _ ->
            settFokusCmd SertifikatTypeaheadId

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



--- VIEW ---


type InputId
    = SertifikatTypeaheadId
    | UtstederId
    | FullførtMånedId
    | LeggTilUtløperId
    | UtløperMånedId
    | BekreftOppsummeringId
    | SlettePåbegyntId
    | LagringFeiletActionId
    | AvbrytSlettingId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        UtstederId ->
            "sertifikat-utsteder-id"

        SertifikatTypeaheadId ->
            "sertifikat-typeahead-id"

        FullførtMånedId ->
            "sertifikat-fullførtmåned-id"

        LeggTilUtløperId ->
            "sertifikat-legg-til-utløper-id"

        UtløperMånedId ->
            "sertifikat-utløpermåned-id"

        BekreftOppsummeringId ->
            "sertifikat-bekreft-oppsummering-id"

        SlettePåbegyntId ->
            "sertifikat-slett-påbegynt-id"

        LagringFeiletActionId ->
            "sertifikat-lagring-feilet-id"

        AvbrytSlettingId ->
            "sertifikat-avbrytt-slett-id"


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
                BrukerInput.typeaheadMedGåVidereKnapp { onAvbryt = VilAvbryteRegistreringen, onGåVidere = VilRegistrereSertifikat }
                    (typeaheadModel
                        |> feilmeldingTypeahead
                        |> maybeHvisTrue visFeilmelding
                        |> Typeahead.toViewElement SertifikatTypeahead.label typeaheadModel
                        |> FrontendModuler.Typeahead.map TypeaheadMsg
                    )

            RegistrerUtsteder input ->
                BrukerInput.inputMedGåVidereKnapp { onAvbryt = VilAvbryteRegistreringen, onGåVidere = VilRegistrereUtsteder }
                    (input.utsteder
                        |> Input.input { label = "Utsteder", msg = OppdatererUtsteder }
                        |> Input.withOnEnter VilRegistrereUtsteder
                        |> Input.withId (inputIdTilString UtstederId)
                    )

            RegistrerFullførtDato fullførtDatoInfo ->
                DatoInput.datoInput
                    { onMånedChange = OppdatererFullførtMåned
                    , måned = fullførtDatoInfo.fullførtMåned
                    , onÅrChange = OppdatererFullførtÅr
                    , år = fullførtDatoInfo.fullførtÅr
                    }
                    |> DatoInput.withFokusId (inputIdTilString FullførtMånedId)
                    |> DatoInput.withFeilmeldingÅr
                        (fullførtDatoInfo.fullførtÅr
                            |> Dato.feilmeldingÅr
                            |> maybeHvisTrue fullførtDatoInfo.visFeilmeldingFullførtÅr
                        )
                    |> DatoInput.withOnBlurÅr ÅrMisterFokus
                    |> BrukerInputMedGåVidereKnapp.datoMånedÅr VilRegistrereFullførtDato
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp VilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            SpørOmUtløpsdatoFinnes _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp SvarerJaTilUtløpsdato "Ja, sertifiseringen utløper"
                        |> Knapp.withId (inputIdTilString LeggTilUtløperId)
                    , Knapp.knapp SvarerNeiTilUtløpsdato "Nei, sertifiseringen utløper ikke"
                    ]

            RegistrerUtløpsdato utløpsdatoInfo ->
                DatoInput.datoInput
                    { onMånedChange = OppdatererUtløperMåned
                    , måned = utløpsdatoInfo.utløperMåned
                    , onÅrChange = OppdatererUtløperÅr
                    , år = utløpsdatoInfo.utløperÅr
                    }
                    |> DatoInput.withFokusId (inputIdTilString UtløperMånedId)
                    |> DatoInput.withFeilmeldingÅr
                        (utløpsdatoInfo.utløperÅr
                            |> Dato.feilmeldingÅr
                            |> maybeHvisTrue utløpsdatoInfo.visFeilmeldingUtløperÅr
                        )
                    |> DatoInput.withOnBlurÅr ÅrMisterFokus
                    |> BrukerInputMedGåVidereKnapp.datoMånedÅr VilRegistrereUtløpsdato
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp VilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

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
                            { onMånedChange = FullførtMåned >> SkjemaEndret
                            , måned = Skjema.fullførtMåned skjema
                            , onÅrChange = FullførtÅr >> SkjemaEndret
                            , år = Skjema.fullførtÅr skjema
                            }
                            |> DatoInput.withLabel "Når fullførte du sertifiseringen?"
                            |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingFullførtÅr skjema)
                            |> DatoInput.withOnBlurÅr (SkjemaEndret FullførtÅrMistetFokus)
                            |> DatoInput.toHtml
                        , if not (Skjema.utløperIkke skjema) then
                            DatoInput.datoInput
                                { onMånedChange = UtløperMåned >> SkjemaEndret
                                , måned = Skjema.utløperMåned skjema
                                , onÅrChange = UtløperÅr >> SkjemaEndret
                                , år = Skjema.utløperÅr skjema
                                }
                                |> DatoInput.withLabel "Når utløper sertifiseringen?"
                                |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingUtløperÅr skjema)
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
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp FerdigMedSertifikat "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreSertifikat "Prøv igjen"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp FerdigMedSertifikat "Gå videre"
                            ]

                    LoggInn ->
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
        [ Knapp.knapp VilLagreSertifikat "Ja, det er riktig"
            |> Knapp.withId (inputIdTilString BekreftOppsummeringId)
        , Knapp.knapp VilEndreOpplysninger "Nei, jeg vil endre"
        , Knapp.knapp VilSlettePåbegynt "Nei, jeg vil slette"
        ]


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


lagreSertifikat : (Result Error (List Sertifikat) -> msg) -> Skjema.ValidertSertifikatSkjema -> Cmd msg
lagreSertifikat msgConstructor skjema =
    case Skjema.id skjema of
        Just id ->
            Api.endreSertifikat msgConstructor skjema id

        Nothing ->
            Api.opprettSertifikat msgConstructor skjema



--- INIT ---


init : DebugStatus -> Posix -> FerdigAnimertMeldingsLogg -> List Sertifikat -> ( Model, Cmd Msg )
init debugStatus sistLagretFraForrigeSeksjon gammelMeldingsLogg sertifikatListe =
    let
        aktivSamtale =
            initSamtaleTypeahead
                |> Tuple.first
                |> RegistrerSertifikatFelt False
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , aktivSamtale = aktivSamtale
        , sertifikatListe = sertifikatListe
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
