module Godkjenning.Seksjon exposing
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
import Godkjenning.Godkjenning as Godkjenning exposing (Godkjenning)
import Godkjenning.GodkjenningTypeahead as GodkjenningTypeahead exposing (GodkjenningTypeahead)
import Godkjenning.Skjema as Skjema exposing (GodkjenningFelt(..), GodkjenningSkjema, Utløpsdato(..), ValidertGodkjenningSkjema)
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
    , godkjenningListe : List Godkjenning
    , debugStatus : DebugStatus
    , sistLagretFraForrigeSeksjon : Posix
    }


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


sistLagret : Model -> Posix
sistLagret (Model model) =
    model.godkjenningListe
        |> List.map Godkjenning.sistEndretDato
        |> nyesteSistLagretVerdi model.sistLagretFraForrigeSeksjon


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix (List Godkjenning) FerdigAnimertMeldingsLogg


type AvsluttetGrunn
    = AvbruttPåbegynt
    | SlettetPåbegynt
    | AnnenAvslutning


type OppsummeringsType
    = FørsteGang
    | EtterEndring
    | AvbrøtSletting


type Samtale
    = RegistrerGodkjenningFelt Bool (Typeahead.Model GodkjenningTypeahead)
    | RegistrerUtsteder UtstederInfo
    | RegistrerFullførtDato FullførtDatoInfo
    | SpørOmUtløpsdatoFinnes SpørOmUtløpsdatoInfo
    | RegistrerUtløpsdato UtløpsdatoInfo
    | VisOppsummering OppsummeringsType ValidertGodkjenningSkjema
    | EndreOpplysninger (Typeahead.Model GodkjenningTypeahead) GodkjenningSkjema
    | BekreftSlettingAvPåbegynt ValidertGodkjenningSkjema
    | LagrerSkjema ValidertGodkjenningSkjema LagreStatus
    | LagringFeilet Http.Error ValidertGodkjenningSkjema
    | BekreftAvbrytingAvRegistreringen Samtale
    | VenterPåAnimasjonFørFullføring (List Godkjenning) AvsluttetGrunn


type alias UtstederInfo =
    { godkjenning : GodkjenningFelt
    , utsteder : String
    }


type alias FullførtDatoInfo =
    { godkjenning : GodkjenningFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : String
    , visFeilmeldingFullførtÅr : Bool
    }


type alias SpørOmUtløpsdatoInfo =
    { godkjenning : GodkjenningFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    }


type alias UtløpsdatoInfo =
    { godkjenning : GodkjenningFelt
    , utsteder : String
    , fullførtMåned : Måned
    , fullførtÅr : År
    , utløperMåned : Måned
    , utløperÅr : String
    , visFeilmeldingUtløperÅr : Bool
    }


godkjenningFeltTilUtsteder : GodkjenningFelt -> UtstederInfo
godkjenningFeltTilUtsteder godkjenningFelt =
    { godkjenning = godkjenningFelt
    , utsteder = ""
    }


utstederTilFullførtDato : UtstederInfo -> FullførtDatoInfo
utstederTilFullførtDato input =
    { godkjenning = input.godkjenning
    , utsteder = input.utsteder
    , fullførtÅr = ""
    , fullførtMåned = Januar
    , visFeilmeldingFullførtÅr = False
    }


fullførtDatoTilSpørOmUtløpsdato : FullførtDatoInfo -> År -> SpørOmUtløpsdatoInfo
fullførtDatoTilSpørOmUtløpsdato input år =
    { godkjenning = input.godkjenning
    , utsteder = input.utsteder
    , fullførtMåned = input.fullførtMåned
    , fullførtÅr = år
    }


spørOmUtløpsdatoInfoTilUtløpsdato : SpørOmUtløpsdatoInfo -> UtløpsdatoInfo
spørOmUtløpsdatoInfoTilUtløpsdato input =
    { godkjenning = input.godkjenning
    , utsteder = input.utsteder
    , fullførtÅr = input.fullførtÅr
    , fullførtMåned = input.fullførtMåned
    , utløperMåned = Januar
    , utløperÅr = ""
    , visFeilmeldingUtløperÅr = False
    }


spørOmUtløpsdatoInfoTilSkjema : SpørOmUtløpsdatoInfo -> ValidertGodkjenningSkjema
spørOmUtløpsdatoInfoTilSkjema input =
    Skjema.initValidertSkjema
        { godkjenningFelt = input.godkjenning
        , utsteder = input.utsteder
        , fullførtMåned = input.fullførtMåned
        , fullførtÅr = input.fullførtÅr
        , utløpsdato = IkkeOppgitt
        , id = Nothing
        }


utløpsdatoInfoTilSkjema : UtløpsdatoInfo -> År -> ValidertGodkjenningSkjema
utløpsdatoInfoTilSkjema info år =
    Skjema.initValidertSkjema
        { godkjenningFelt = info.godkjenning
        , utsteder = info.utsteder
        , fullførtMåned = info.fullførtMåned
        , fullførtÅr = info.fullførtÅr
        , utløpsdato = Oppgitt info.utløperMåned år
        , id = Nothing
        }



--- UPDATE ---


type Msg
    = TypeaheadMsg (Typeahead.Msg GodkjenningTypeahead)
    | HentetTypeahead Typeahead.Query (Result Http.Error (List GodkjenningTypeahead))
    | VilRegistrereGodkjenning
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
    | VilLagreGodkjenning
    | VilEndreOpplysninger
    | SkjemaEndret SkjemaEndring
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | VilLagreEndretSkjema
    | GodkjenningLagret (Result Http.Error (List Godkjenning))
    | VilAvbryteRegistreringen
    | BekrefterAvbrytingAvRegistrering
    | VilIkkeAvbryteRegistreringen
    | FerdigMedGodkjenning
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
                RegistrerGodkjenningFelt visFeilmelding typeaheadModel ->
                    updateSamtaleTypeahead model visFeilmelding typeaheadMsg typeaheadModel

                EndreOpplysninger typeaheadModel skjema ->
                    let
                        ( nyTypeaheadModel, status ) =
                            Typeahead.update GodkjenningTypeahead.label typeaheadMsg typeaheadModel
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> tilGodkjenningFelt
                            |> Skjema.oppdaterGodkjenning skjema
                            |> Skjema.visFeilmeldingGodkjenningFelt (Typeahead.inputStatus status == InputBlurred)
                            |> EndreOpplysninger nyTypeaheadModel
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , case Typeahead.getSuggestionsStatus status of
                            GetSuggestionsForInput query ->
                                Api.getGodkjenningTypeahead HentetTypeahead query

                            DoNothing ->
                                Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead query result ->
            case model.aktivSamtale of
                RegistrerGodkjenningFelt visFeilmelding typeaheadModel ->
                    ( result
                        |> Typeahead.updateSuggestions GodkjenningTypeahead.label typeaheadModel query
                        |> RegistrerGodkjenningFelt visFeilmelding
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hent GodkjenningTypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndreOpplysninger typeaheadModel skjema ->
                    ( EndreOpplysninger (Typeahead.updateSuggestions GodkjenningTypeahead.label typeaheadModel query result) skjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hent GodkjenningTypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilRegistrereGodkjenning ->
            case model.aktivSamtale of
                RegistrerGodkjenningFelt _ typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just godkjenning ->
                            godkjenning
                                |> GodkjenningFraTypeahead
                                |> brukerVelgerGodkjenningFelt model

                        Nothing ->
                            brukerVilRegistrereFritekstGodkjenning model typeaheadModel

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
                RegistrerGodkjenningFelt _ typeaheadModel ->
                    visFeilmeldingRegistrerGodkjenning model typeaheadModel

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
                VisOppsummering _ validertGodkjenningSkjema ->
                    updateEtterVilEndreSkjema model msg validertGodkjenningSkjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SkjemaEndret skjemaEndring ->
            case model.aktivSamtale of
                EndreOpplysninger typeaheadModel godkjenningSkjema ->
                    ( godkjenningSkjema
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
                    ( VenterPåAnimasjonFørFullføring model.godkjenningListe SlettetPåbegynt
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

        VilLagreGodkjenning ->
            case model.aktivSamtale of
                VisOppsummering _ skjema ->
                    updateEtterLagreKnappTrykket model msg skjema

                LagringFeilet error skjema ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSkjema skjema
                        |> oppdaterSamtale model (SvarFraMsg msg)
                    , lagreGodkjenning GodkjenningLagret skjema
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        GodkjenningLagret result ->
            case model.aktivSamtale of
                LagrerSkjema skjema lagreStatus ->
                    case result of
                        Ok godkjenninger ->
                            let
                                oppdatertMeldingslogg =
                                    if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Nå er godkjenningen lagret 👍" ] ]

                                    else
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Nå er godkjenningen lagret 👍" ] ]
                            in
                            ( VenterPåAnimasjonFørFullføring godkjenninger AnnenAvslutning
                                |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg, godkjenningListe = godkjenninger } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSkjema skjema
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , lagreGodkjenning GodkjenningLagret skjema
                                    )
                                        |> IkkeFerdig

                                else
                                    ( skjema
                                        |> LagringFeilet error
                                        |> oppdaterSamtale model IngenNyeMeldinger
                                    , skjema
                                        |> Skjema.encode
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre godkjenning" error
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
                                        |> Api.logErrorWithRequestBody ErrorLogget "Lagre godkjenning" error
                                    ]
                                )
                                    |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none )
                        |> IkkeFerdig

        VilAvbryteRegistreringen ->
            case model.aktivSamtale of
                RegistrerGodkjenningFelt _ _ ->
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

        FerdigMedGodkjenning ->
            case model.aktivSamtale of
                LagringFeilet _ _ ->
                    ( VenterPåAnimasjonFørFullføring model.godkjenningListe AnnenAvslutning
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
                                    , lagreGodkjenning GodkjenningLagret skjema
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


initSamtaleTypeahead : ( Typeahead.Model GodkjenningTypeahead, Typeahead.Query )
initSamtaleTypeahead =
    Typeahead.init
        { value = ""
        , label = "Godkjenning"
        , id = inputIdTilString GodkjenningTypeaheadId
        , toString = GodkjenningTypeahead.label
        }


initSkjemaTypeahead : ValidertGodkjenningSkjema -> ( Typeahead.Model GodkjenningTypeahead, Typeahead.Query )
initSkjemaTypeahead skjema =
    case Skjema.godkjenningFeltValidert skjema of
        GodkjenningFraTypeahead godkjenningTypeahead ->
            Typeahead.initWithSelected
                { selected = godkjenningTypeahead
                , label = "Sertifisering eller godkjenning"
                , id = inputIdTilString GodkjenningTypeaheadId
                , toString = GodkjenningTypeahead.label
                }

        Egendefinert inputValue ->
            Typeahead.init
                { value = inputValue
                , label = "Sertifisering eller godkjenning"
                , id = inputIdTilString GodkjenningTypeaheadId
                , toString = GodkjenningTypeahead.label
                }


godkjenningFeltFraTypeaheadModel : Typeahead.Model GodkjenningTypeahead -> Maybe GodkjenningFelt
godkjenningFeltFraTypeaheadModel typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just godkjenning ->
            Just (GodkjenningFraTypeahead godkjenning)

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
        |> VenterPåAnimasjonFørFullføring model.godkjenningListe
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


brukerVilRegistrereFritekstGodkjenning : ModelInfo -> Typeahead.Model GodkjenningTypeahead -> SamtaleStatus
brukerVilRegistrereFritekstGodkjenning model typeaheadModel =
    case godkjenningFeltFraTypeaheadModel typeaheadModel of
        Just godkjenningFelt ->
            brukerVelgerGodkjenningFelt model godkjenningFelt

        Nothing ->
            visFeilmeldingRegistrerGodkjenning model typeaheadModel


visFeilmeldingRegistrerGodkjenning : ModelInfo -> Typeahead.Model GodkjenningTypeahead -> SamtaleStatus
visFeilmeldingRegistrerGodkjenning model typeaheadModel =
    ( typeaheadModel
        |> RegistrerGodkjenningFelt True
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


tilGodkjenningFelt : Typeahead.Model GodkjenningTypeahead -> GodkjenningFelt
tilGodkjenningFelt typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just godkjenningTypeahead ->
            GodkjenningFraTypeahead godkjenningTypeahead

        Nothing ->
            Egendefinert (Typeahead.inputValue typeaheadModel)


updateSamtaleTypeahead : ModelInfo -> Bool -> Typeahead.Msg GodkjenningTypeahead -> Typeahead.Model GodkjenningTypeahead -> SamtaleStatus
updateSamtaleTypeahead model visFeilmelding msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update GodkjenningTypeahead.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case godkjenningFeltFraTypeaheadModel nyTypeaheadModel of
                Just godkjenningFelt ->
                    brukerVelgerGodkjenningFelt model godkjenningFelt

                Nothing ->
                    visFeilmeldingRegistrerGodkjenning model nyTypeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerGodkjenningFelt visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerGodkjenningFelt visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput string ->
                        Api.getGodkjenningTypeahead HentetTypeahead string

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerGodkjenningFelt visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView GodkjenningTypeahead.label Nothing
                    |> Cmd.map TypeaheadMsg
                )


feilmeldingTypeahead : Typeahead.Model GodkjenningTypeahead -> Maybe String
feilmeldingTypeahead typeaheadModel =
    case godkjenningFeltFraTypeaheadModel typeaheadModel of
        Just _ ->
            Nothing

        Nothing ->
            Just "Velg eller skriv inn godkjenning"


oppdaterSkjema : SkjemaEndring -> GodkjenningSkjema -> GodkjenningSkjema
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
                VenterPåAnimasjonFørFullføring godkjenningListe _ ->
                    Ferdig (sistLagret (Model model)) godkjenningListe ferdigAnimertSamtale

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


brukerVelgerGodkjenningFelt : ModelInfo -> GodkjenningFelt -> SamtaleStatus
brukerVelgerGodkjenningFelt info godkjenningFelt =
    ( godkjenningFelt
        |> godkjenningFeltTilUtsteder
        |> RegistrerUtsteder
        |> oppdaterSamtale info (ManueltSvar (Melding.svar [ godkjenningFeltTilString godkjenningFelt ]))
    , lagtTilSpørsmålCmd info.debugStatus
    )
        |> IkkeFerdig


godkjenningFeltTilString : GodkjenningFelt -> String
godkjenningFeltTilString godkjenningFelt =
    case godkjenningFelt of
        GodkjenningFraTypeahead godkjenningTypeahead ->
            GodkjenningTypeahead.label godkjenningTypeahead

        Egendefinert inputValue ->
            inputValue


updateEtterVilEndreSkjema : ModelInfo -> Msg -> ValidertGodkjenningSkjema -> SamtaleStatus
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
        , Api.getGodkjenningTypeahead HentetTypeahead query
        ]
    )
        |> IkkeFerdig


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertGodkjenningSkjema -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema =
    ( LagreStatus.init
        |> LagrerSkjema skjema
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagreGodkjenning GodkjenningLagret skjema
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
samtaleTilMeldingsLogg godkjenningSeksjon =
    case godkjenningSeksjon of
        RegistrerGodkjenningFelt _ _ ->
            [ Melding.spørsmål [ "Hva slags godkjenning har du?" ]
            , Melding.spørsmål [ "Kanskje du har truckførerbevis T1, eller noe helt annet? 😊" ]
            ]

        RegistrerUtsteder _ ->
            [ Melding.spørsmål [ "Hvilken organisasjon har utstedt godkjenningen?" ]
            , Melding.spørsmål [ "Er du usikker på hvem som har ansvar for ? Det står ofte på beviset ditt." ]
            ]

        RegistrerFullførtDato _ ->
            [ Melding.spørsmål [ "Når fullførte du godkjenningen?" ]
            ]

        SpørOmUtløpsdatoFinnes _ ->
            [ Melding.spørsmål [ "Har godkjenningen en utløpsdato?" ]
            ]

        RegistrerUtløpsdato _ ->
            [ Melding.spørsmål [ "Når utløper godkjenningen din?" ]
            ]

        VisOppsummering oppsummeringsType skjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    [ Melding.spørsmål [ "Ok, da lar jeg godkjenningen stå." ]
                    , oppsummeringsSpørsmål skjema
                    ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål skjema ]

        EndreOpplysninger _ _ ->
            [ Melding.spørsmål [ "Endre informasjonen i feltene under." ] ]

        BekreftSlettingAvPåbegynt _ ->
            [ Melding.spørsmål [ "Er du sikker på at du vil slette denne godkjenningen?" ] ]

        LagrerSkjema _ _ ->
            []

        LagringFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre godkjenning" } ]

        BekreftAvbrytingAvRegistreringen _ ->
            [ Melding.spørsmål [ "Hvis du avbryter, blir ikke godkjenningen lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

        VenterPåAnimasjonFørFullføring _ avsluttetGrunn ->
            case avsluttetGrunn of
                AvbruttPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg avbrutt. Vil du legge inn flere kategorier?" ] ]

                SlettetPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg slettet godkjenningen. Vil du legge inn flere kategorier?" ] ]

                AnnenAvslutning ->
                    [ Melding.spørsmål [ "Vil du legge inn flere kategorier?" ] ]


validertSkjemaTilSetninger : ValidertGodkjenningSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ "Godkjenning: " ++ (validertSkjema |> Skjema.godkjenningString)
    , "Utsteder: " ++ Skjema.utsteder skjema
    , "Fullført: " ++ Dato.datoTilString (Skjema.fullførtMåned skjema) (Skjema.fullførtÅrValidert validertSkjema)
    , "Utløper: " ++ utløpsdatoTilString (Skjema.utløpsdatoValidert validertSkjema)
    ]


oppsummeringsSpørsmål : ValidertGodkjenningSkjema -> Melding
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
        RegistrerGodkjenningFelt _ _ ->
            settFokusCmd GodkjenningTypeaheadId

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
            settFokusCmd GodkjenningTypeaheadId

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
    = GodkjenningTypeaheadId
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
            "godkjenning-utsteder-id"

        GodkjenningTypeaheadId ->
            "godkjenning-typeahead-id"

        FullførtMånedId ->
            "godkjenning-fullførtmåned-id"

        LeggTilUtløperId ->
            "godkjenning-legg-til-utløper-id"

        UtløperMånedId ->
            "godkjenning-utløpermåned-id"

        BekreftOppsummeringId ->
            "godkjenning-bekreft-oppsummering-id"

        SlettePåbegyntId ->
            "godkjenning-slett-påbegynt-id"

        LagringFeiletActionId ->
            "godkjenning-lagring-feilet-id"

        AvbrytSlettingId ->
            "godkjenning-avbrytt-slett-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            RegistrerGodkjenningFelt visFeilmelding typeaheadModel ->
                BrukerInput.typeaheadMedGåVidereKnapp { onAvbryt = VilAvbryteRegistreringen, onGåVidere = VilRegistrereGodkjenning }
                    (typeaheadModel
                        |> feilmeldingTypeahead
                        |> maybeHvisTrue visFeilmelding
                        |> Typeahead.toViewElement GodkjenningTypeahead.label typeaheadModel
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
                    [ Knapp.knapp SvarerJaTilUtløpsdato "Ja, godkjenningen utløper"
                        |> Knapp.withId (inputIdTilString LeggTilUtløperId)
                    , Knapp.knapp SvarerNeiTilUtløpsdato "Nei, godkjenningen utløper ikke"
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
                        |> Skjema.feilmeldingGodkjenningFelt
                        |> Typeahead.view GodkjenningTypeahead.label typeaheadModel
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
                            |> DatoInput.withLabel "Når fullførte du godkjenningen?"
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
                                |> DatoInput.withLabel "Når utløper godkjenningen?"
                                |> DatoInput.withFeilmeldingÅr (Skjema.feilmeldingUtløperÅr skjema)
                                |> DatoInput.withOnBlurÅr (SkjemaEndret UtløperÅrMistetFokus)
                                |> DatoInput.toHtml

                          else
                            text ""
                        ]
                    , skjema
                        |> Skjema.utløperIkke
                        |> Checkbox.checkbox "Godkjenningen utløper ikke" (SkjemaEndret UtløperIkkeToggled)
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
                            [ Knapp.knapp FerdigMedGodkjenning "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreGodkjenning "Prøv igjen"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp FerdigMedGodkjenning "Gå videre"
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
        [ Knapp.knapp VilLagreGodkjenning "Ja, det er riktig"
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


lagreGodkjenning : (Result Error (List Godkjenning) -> msg) -> Skjema.ValidertGodkjenningSkjema -> Cmd msg
lagreGodkjenning msgConstructor skjema =
    case Skjema.id skjema of
        Just id ->
            Api.endreGodkjenning msgConstructor skjema id

        Nothing ->
            Api.opprettGodkjenning msgConstructor skjema



--- INIT ---


init : DebugStatus -> Posix -> FerdigAnimertMeldingsLogg -> List Godkjenning -> ( Model, Cmd Msg )
init debugStatus sistLagretFraForrigeSeksjon gammelMeldingsLogg godkjenningListe =
    let
        aktivSamtale =
            initSamtaleTypeahead
                |> Tuple.first
                |> RegistrerGodkjenningFelt False
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , aktivSamtale = aktivSamtale
        , godkjenningListe = godkjenningListe
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
