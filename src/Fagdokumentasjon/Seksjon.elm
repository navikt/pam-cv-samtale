module Fagdokumentasjon.Seksjon exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , initAutorisasjon
    , initFagbrev
    , initMesterbrev
    , meldingsLogg
    , subscriptions
    , update
    , viewBrukerInput
    )

import Api
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Cv.Fagdokumentasjon exposing (Fagdokumentasjon, FagdokumentasjonType(..))
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Fagdokumentasjon.Konsept as Konsept exposing (Konsept)
import Fagdokumentasjon.Skjema as Skjema exposing (FagdokumentasjonSkjema, ValidertFagdokumentasjonSkjema)
import Feilmelding
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
import FrontendModuler.Knapp as Knapp
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Textarea as Textarea
import FrontendModuler.Typeahead
import Html exposing (Html)
import Http exposing (Error(..))
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Process
import Result.Extra as Result
import Task
import Typeahead.Typeahead as Typeahead exposing (GetSuggestionStatus(..), InputStatus(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , fagdokumentasjonListe : List Fagdokumentasjon
    , fagdokumentasjonType : FagdokumentasjonType
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
    = RegistrerKonsept Bool (Typeahead.Model Konsept)
    | RegistrerBeskrivelse Bool BeskrivelseInfo
    | Oppsummering OppsummeringsType ValidertFagdokumentasjonSkjema
    | EndrerOppsummering (Typeahead.Model Konsept) FagdokumentasjonSkjema
    | BekreftSlettingAvPåbegynt ValidertFagdokumentasjonSkjema
    | Lagrer LagreStatus ValidertFagdokumentasjonSkjema
    | LagringFeilet ValidertFagdokumentasjonSkjema Http.Error
    | BekreftAvbrytingAvRegistreringen Samtale
    | VenterPåAnimasjonFørFullføring (List Fagdokumentasjon) AvsluttetGrunn


type LagreStatus
    = LagrerFørsteGang
    | LagrerPåNyttEtterError Http.Error
    | ForsøkÅLagrePåNyttEtterDetteForsøket


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Fagdokumentasjon) FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


type alias BeskrivelseInfo =
    { konsept : Konsept
    , beskrivelse : String
    }


forrigetilBeskrivelseInfo : Konsept -> BeskrivelseInfo
forrigetilBeskrivelseInfo konseptTypeahead =
    { konsept = konseptTypeahead, beskrivelse = "" }



--- UPDATE ---


type Msg
    = TypeaheadMsg (Typeahead.Msg Konsept)
    | HentetTypeahead Typeahead.Query (Result Http.Error (List Konsept))
    | TimeoutEtterAtFeltMistetFokus
    | BrukerVilRegistrereKonsept
    | VilSeEksempel
    | BrukerVilRegistrereFagdokumentasjonBeskrivelse
    | OppdaterFagdokumentasjonBeskrivelse String
    | BrukerVilLagreIOppsummeringen
    | BrukerVilEndreOppsummeringen
    | VilSlettePåbegynt
    | BekrefterSlettPåbegynt
    | AngrerSlettPåbegynt
    | BrukerLagrerSkjema
    | FagbrevSendtTilApi (Result Http.Error (List Fagdokumentasjon))
    | BrukerVilPrøveÅLagrePåNytt
    | BrukerVilIkkePrøveÅLagrePåNytt
    | BrukerVilAvbryteRegistreringen
    | BrukerBekrefterAvbrytingAvRegistrering
    | BrukerVilIkkeAvbryteRegistreringen
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | WindowEndrerVisibility Visibility
    | FokusSatt (Result Dom.Error ())
    | ErrorLogget (Result Http.Error ())


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        TypeaheadMsg typeaheadMsg ->
            case model.aktivSamtale of
                RegistrerKonsept visFeilmelding typeaheadModel ->
                    updateSamtaleTypeahead model visFeilmelding typeaheadMsg typeaheadModel

                EndrerOppsummering typeaheadModel skjema ->
                    let
                        ( nyTypeaheadModel, status ) =
                            Typeahead.update Konsept.label typeaheadMsg typeaheadModel
                    in
                    IkkeFerdig
                        ( nyTypeaheadModel
                            |> Typeahead.selected
                            |> Skjema.oppdaterKonsept skjema
                            |> Skjema.gjørFeilmeldingKonseptSynlig (Typeahead.inputStatus status == InputBlurred)
                            |> EndrerOppsummering nyTypeaheadModel
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , case Typeahead.getSuggestionsStatus status of
                            GetSuggestionsForInput query ->
                                skjema
                                    |> Skjema.fagdokumentasjonType
                                    |> hentTypeaheadSuggestions query

                            DoNothing ->
                                Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        HentetTypeahead query result ->
            case model.aktivSamtale of
                RegistrerKonsept visFeilmelding typeaheadModel ->
                    ( result
                        |> Typeahead.updateSuggestions Konsept.label typeaheadModel query
                        |> RegistrerKonsept visFeilmelding
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente FagbrevTypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                EndrerOppsummering typeaheadModel skjema ->
                    ( EndrerOppsummering (Typeahead.updateSuggestions Konsept.label typeaheadModel query result) skjema
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , result
                        |> Result.error
                        |> Maybe.map (logFeilmelding "Hente AutorisasjonTypeahead")
                        |> Maybe.withDefault Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        TimeoutEtterAtFeltMistetFokus ->
            case model.aktivSamtale of
                RegistrerKonsept _ typeaheadModel ->
                    visFeilmeldingRegistrerKonsept model typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereKonsept ->
            case model.aktivSamtale of
                RegistrerKonsept _ typeaheadModel ->
                    case Typeahead.selected typeaheadModel of
                        Just konsept ->
                            brukerVilRegistrereKonsept model msg konsept

                        Nothing ->
                            visFeilmeldingRegistrerKonsept model typeaheadModel

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        VilSeEksempel ->
            case model.aktivSamtale of
                RegistrerBeskrivelse _ beskrivelseinfo ->
                    let
                        oppdatertMeldingslogg =
                            model.seksjonsMeldingsLogg
                                |> MeldingsLogg.leggTilSvar (svarFraBrukerInput model msg)
                                |> MeldingsLogg.leggTilSpørsmål (eksemplerPåFagdokumentasjon model.fagdokumentasjonType)
                    in
                    IkkeFerdig
                        ( beskrivelseinfo
                            |> RegistrerBeskrivelse False
                            |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } IngenNyeMeldinger
                        , lagtTilSpørsmålCmd model.debugStatus
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilRegistrereFagdokumentasjonBeskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse _ info ->
                    case feilmeldingBeskrivelsesfelt info.beskrivelse of
                        Nothing ->
                            ( Skjema.initValidertSkjema model.fagdokumentasjonType info.konsept info.beskrivelse
                                |> Oppsummering FørsteGang
                                |> oppdaterSamtale model (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Just _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilLagreIOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering _ skjema ->
                    LagrerFørsteGang
                        |> updateEtterLagreKnappTrykket model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilEndreOppsummeringen ->
            case model.aktivSamtale of
                Oppsummering _ validertSkjema ->
                    initRedigeringAvValidertSkjema model msg validertSkjema

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
                    ( SlettetPåbegynt
                        |> VenterPåAnimasjonFørFullføring model.fagdokumentasjonListe
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

        BrukerLagrerSkjema ->
            case model.aktivSamtale of
                EndrerOppsummering typeaheadModel skjema ->
                    case Skjema.validertSkjema skjema of
                        Just validertSkjema ->
                            IkkeFerdig
                                ( validertSkjema
                                    |> Oppsummering EtterEndring
                                    |> oppdaterSamtale model (ManueltSvar (Melding.svar (validertSkjemaTilSetninger validertSkjema)))
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Nothing ->
                            IkkeFerdig
                                ( skjema
                                    |> Skjema.gjørFeilmeldingKonseptSynlig True
                                    |> EndrerOppsummering typeaheadModel
                                    |> oppdaterSamtale model IngenNyeMeldinger
                                , Cmd.none
                                )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        OppdaterFagdokumentasjonBeskrivelse beskrivelse ->
            case model.aktivSamtale of
                RegistrerBeskrivelse medEksempelKnapp info ->
                    ( { info | beskrivelse = beskrivelse }
                        |> RegistrerBeskrivelse medEksempelKnapp
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                EndrerOppsummering typeaheadModel skjema ->
                    ( skjema
                        |> Skjema.oppdaterBeskrivelse beskrivelse
                        |> EndrerOppsummering typeaheadModel
                        |> oppdaterSamtale model IngenNyeMeldinger
                    , Cmd.none
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        FagbrevSendtTilApi result ->
            case model.aktivSamtale of
                Lagrer lagreStatus skjema ->
                    case result of
                        Ok fagdokumentasjoner ->
                            let
                                oppdatertMeldingslogg =
                                    if lagringFeiletTidligerePåGrunnAvInnlogging lagreStatus then
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                            |> MeldingsLogg.leggTilSpørsmål
                                                [ meldingForLagringSuccess skjema ]

                                    else
                                        model.seksjonsMeldingsLogg
                                            |> MeldingsLogg.leggTilSpørsmål
                                                [ meldingForLagringSuccess skjema ]
                            in
                            ( VenterPåAnimasjonFørFullføring fagdokumentasjoner AnnenAvslutning
                                |> oppdaterSamtale { model | seksjonsMeldingsLogg = oppdatertMeldingslogg } UtenSvar
                            , lagtTilSpørsmålCmd model.debugStatus
                            )
                                |> IkkeFerdig

                        Err error ->
                            ( error
                                |> LagringFeilet skjema
                                |> oppdaterSamtale model UtenSvar
                            , Cmd.batch
                                [ lagtTilSpørsmålCmd model.debugStatus
                                , logFeilmelding "Lagre fagbrev" error
                                ]
                            )
                                |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case model.aktivSamtale of
                        LagringFeilet skjema ((BadStatus 401) as error) ->
                            ( skjema
                                |> Lagrer (LagrerPåNyttEtterError error)
                                |> oppdaterSamtale model UtenSvar
                            , Api.postFagdokumentasjon FagbrevSendtTilApi skjema
                            )
                                |> IkkeFerdig

                        Lagrer (LagrerPåNyttEtterError (BadStatus 401)) skjema ->
                            ( skjema
                                |> Lagrer ForsøkÅLagrePåNyttEtterDetteForsøket
                                |> oppdaterSamtale model IngenNyeMeldinger
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        _ ->
                            IkkeFerdig ( Model model, Cmd.none )

                Hidden ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilPrøveÅLagrePåNytt ->
            case model.aktivSamtale of
                LagringFeilet skjema error ->
                    error
                        |> LagrerPåNyttEtterError
                        |> updateEtterLagreKnappTrykket model msg skjema

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        BrukerVilIkkePrøveÅLagrePåNytt ->
            IkkeFerdig
                ( VenterPåAnimasjonFørFullføring model.fagdokumentasjonListe AnnenAvslutning
                    |> oppdaterSamtale model (SvarFraMsg msg)
                , lagtTilSpørsmålCmd model.debugStatus
                )

        BrukerVilAvbryteRegistreringen ->
            case model.aktivSamtale of
                RegistrerKonsept _ _ ->
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
                                            , samtaleTilMeldingsLogg model.fagdokumentasjonType samtaleStegFørAvbryting
                                            ]
                                        )
                        }
                    , lagtTilSpørsmålCmd model.debugStatus
                    )
                        |> IkkeFerdig

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg model.seksjonsMeldingsLogg
                |> updateEtterFullførtMelding model

        FokusSatt _ ->
            case model.aktivSamtale of
                EndrerOppsummering typeaheadModel skjema ->
                    IkkeFerdig
                        ( EndrerOppsummering (Typeahead.hideSuggestions typeaheadModel) skjema
                            |> oppdaterSamtale model IngenNyeMeldinger
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        ErrorLogget _ ->
            IkkeFerdig ( Model model, Cmd.none )


avbrytRegistrering : ModelInfo -> Msg -> SamtaleStatus
avbrytRegistrering model msg =
    ( AvbruttPåbegynt
        |> VenterPåAnimasjonFørFullføring model.fagdokumentasjonListe
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


validertSkjemaTilSetninger : ValidertFagdokumentasjonSkjema -> List String
validertSkjemaTilSetninger validertSkjema =
    let
        skjema =
            Skjema.tilUvalidertSkjema validertSkjema
    in
    [ typeaheadLabel (Skjema.fagdokumentasjonType skjema) ++ ": " ++ Skjema.konseptStringFraValidertSkjema validertSkjema
    , "Beskrivelse: " ++ Skjema.beskrivelseFraValidertSkjema validertSkjema
    ]


initRedigeringAvValidertSkjema : ModelInfo -> Msg -> ValidertFagdokumentasjonSkjema -> SamtaleStatus
initRedigeringAvValidertSkjema model msg validertSkjema =
    let
        ( typeaheadModel, query ) =
            initSkjemaTypeaheadFraValidertSkjema validertSkjema
    in
    ( validertSkjema
        |> Skjema.tilUvalidertSkjema
        |> EndrerOppsummering typeaheadModel
        |> oppdaterSamtale model (SvarFraMsg msg)
    , Cmd.batch
        [ lagtTilSpørsmålCmd model.debugStatus
        , validertSkjema
            |> Skjema.tilUvalidertSkjema
            |> Skjema.fagdokumentasjonType
            |> hentTypeaheadSuggestions query
        ]
    )
        |> IkkeFerdig


initSamtaleTypeahead : FagdokumentasjonType -> ( Typeahead.Model Konsept, Typeahead.Query )
initSamtaleTypeahead fagdokumentasjonType =
    Typeahead.init
        { value = ""
        , label = typeaheadLabel fagdokumentasjonType
        , id = inputIdTilString RegistrerKonseptInput
        , toString = Konsept.label
        }


initSkjemaTypeaheadFraValidertSkjema : ValidertFagdokumentasjonSkjema -> ( Typeahead.Model Konsept, Typeahead.Query )
initSkjemaTypeaheadFraValidertSkjema skjema =
    skjema
        |> Skjema.konseptFraValidertSkjema
        |> initSkjemaTypeaheadFraKonsept (skjema |> Skjema.tilUvalidertSkjema |> Skjema.fagdokumentasjonType)


initSkjemaTypeaheadFraKonsept : FagdokumentasjonType -> Konsept -> ( Typeahead.Model Konsept, Typeahead.Query )
initSkjemaTypeaheadFraKonsept fagdokumentasjonType konsept =
    Typeahead.initWithSelected
        { selected = konsept
        , label = typeaheadLabel fagdokumentasjonType
        , id = inputIdTilString RegistrerKonseptInput
        , toString = Konsept.label
        }


updateSamtaleTypeahead : ModelInfo -> Bool -> Typeahead.Msg Konsept -> Typeahead.Model Konsept -> SamtaleStatus
updateSamtaleTypeahead model visFeilmelding msg typeaheadModel =
    let
        ( nyTypeaheadModel, status ) =
            Typeahead.update Konsept.label msg typeaheadModel
    in
    case Typeahead.inputStatus status of
        Typeahead.Submit ->
            case Typeahead.selected nyTypeaheadModel of
                Just konsept ->
                    brukerVilRegistrereKonsept model (TypeaheadMsg msg) konsept

                Nothing ->
                    visFeilmeldingRegistrerKonsept model nyTypeaheadModel

        Typeahead.InputBlurred ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerKonsept visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , mistetFokusCmd
                )

        Typeahead.NoChange ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerKonsept visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , case Typeahead.getSuggestionsStatus status of
                    GetSuggestionsForInput query ->
                        hentTypeaheadSuggestions query model.fagdokumentasjonType

                    DoNothing ->
                        Cmd.none
                )

        NewActiveElement ->
            IkkeFerdig
                ( nyTypeaheadModel
                    |> RegistrerKonsept visFeilmelding
                    |> oppdaterSamtale model IngenNyeMeldinger
                , nyTypeaheadModel
                    |> Typeahead.scrollActiveSuggestionIntoView Konsept.label Nothing
                    |> Cmd.map TypeaheadMsg
                )


visFeilmeldingRegistrerKonsept : ModelInfo -> Typeahead.Model Konsept -> SamtaleStatus
visFeilmeldingRegistrerKonsept model typeaheadModel =
    ( typeaheadModel
        |> RegistrerKonsept True
        |> oppdaterSamtale model IngenNyeMeldinger
    , Cmd.none
    )
        |> IkkeFerdig


brukerVilRegistrereKonsept : ModelInfo -> Msg -> Konsept -> SamtaleStatus
brukerVilRegistrereKonsept model msg konsept =
    ( konsept
        |> forrigetilBeskrivelseInfo
        |> RegistrerBeskrivelse True
        |> oppdaterSamtale model (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )
        |> IkkeFerdig


feilmeldingstekstIkkeValgtKonsept : FagdokumentasjonType -> String
feilmeldingstekstIkkeValgtKonsept fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            "Velg et svennebrev/fagbrev fra listen med forslag som kommer opp"

        Mesterbrev ->
            "Velg et mesterbrev fra listen med forslag som kommer opp"

        Autorisasjon ->
            "Velg en autorisasjon fra listen med forslag som kommer opp"


hentTypeaheadSuggestions : Typeahead.Query -> FagdokumentasjonType -> Cmd Msg
hentTypeaheadSuggestions query fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            Api.getFagbrevTypeahead HentetTypeahead query

        Mesterbrev ->
            Api.getMesterbrevTypeahead HentetTypeahead query

        Autorisasjon ->
            Api.getAutorisasjonTypeahead HentetTypeahead query


updateEtterLagreKnappTrykket : ModelInfo -> Msg -> ValidertFagdokumentasjonSkjema -> LagreStatus -> SamtaleStatus
updateEtterLagreKnappTrykket model msg skjema lagreStatus =
    IkkeFerdig
        ( skjema
            |> Lagrer lagreStatus
            |> oppdaterSamtale model (SvarFraMsg msg)
        , Cmd.batch
            [ Api.postFagdokumentasjon FagbrevSendtTilApi skjema
            , lagtTilSpørsmålCmd model.debugStatus
            ]
        )


meldingForLagringSuccess : ValidertFagdokumentasjonSkjema -> Melding
meldingForLagringSuccess skjema =
    let
        fagdokumentasjonType =
            skjema
                |> Skjema.tilUvalidertSkjema
                |> Skjema.fagdokumentasjonType
    in
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            Melding.spørsmål [ "Nå er det lagret! Så bra at du har fagbrev/svennebrev, arbeidsgivere etterspør det." ]

        Mesterbrev ->
            Melding.spørsmål [ "Nå er det lagret! Så bra at du har mesterbrev, det er mangel på jobbsøkere med mesterbrev." ]

        Autorisasjon ->
            Melding.spørsmål [ "Nå er det lagret. Så bra at du har autorisasjon!" ]


lagringFeiletTidligerePåGrunnAvInnlogging : LagreStatus -> Bool
lagringFeiletTidligerePåGrunnAvInnlogging lagreStatus =
    case lagreStatus of
        LagrerFørsteGang ->
            False

        LagrerPåNyttEtterError error ->
            ErrorHåndtering.operasjonEtterError error == LoggInn

        ForsøkÅLagrePåNyttEtterDetteForsøket ->
            True


updateEtterFullførtMelding : ModelInfo -> ( MeldingsLogg, Cmd SamtaleAnimasjon.Msg ) -> SamtaleStatus
updateEtterFullførtMelding model ( nyMeldingsLogg, cmd ) =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring fagdokumentasjonListe _ ->
                    Ferdig fagdokumentasjonListe ferdigAnimertSamtale

                _ ->
                    ( Model { model | seksjonsMeldingsLogg = nyMeldingsLogg }
                    , Cmd.batch
                        [ Cmd.map SamtaleAnimasjonMsg cmd
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
            , Cmd.map SamtaleAnimasjonMsg cmd
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
        |> Maybe.map (Api.logError ErrorLogget)
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
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model.fagdokumentasjonType samtale)

                    ManueltSvar melding ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar melding
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model.fagdokumentasjonType samtale)

                    UtenSvar ->
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg model.fagdokumentasjonType samtale)
        }


samtaleTilMeldingsLogg : FagdokumentasjonType -> Samtale -> List Melding
samtaleTilMeldingsLogg fagdokumentasjonType fagbrevSeksjon =
    case fagbrevSeksjon of
        RegistrerKonsept _ _ ->
            case fagdokumentasjonType of
                SvennebrevFagbrev ->
                    [ Melding.spørsmål [ "Hva er navnet på fagbrevet/svennebrevet ditt?" ]
                    , Melding.spørsmål [ "Begynn å skriv inn fagbrevet/svennebrevet. Velg et av forslagene fra listen som kommer opp." ]
                    ]

                Mesterbrev ->
                    [ Melding.spørsmål [ "Hva er navnet på mesterbrevet ditt?" ]
                    , Melding.spørsmål [ "Begynn å skriv inn mesterbrevet. Velg fra listen med forslag som kommer opp." ]
                    ]

                Autorisasjon ->
                    [ Melding.spørsmål [ "Hva er navnet på autorisasjonen din?" ]
                    , Melding.spørsmål [ "Begynn å skriv inn autorisasjonen din. Velg fra listen med forslag som kommer opp." ]
                    ]

        RegistrerBeskrivelse _ _ ->
            case fagdokumentasjonType of
                SvennebrevFagbrev ->
                    [ Melding.spørsmål [ "Beskriv kort fagbrevet/svennebrevet ditt." ]
                    ]

                Mesterbrev ->
                    [ Melding.spørsmål [ "Beskriv kort mesterbrevet ditt." ]
                    ]

                Autorisasjon ->
                    [ Melding.spørsmål [ "Beskriv kort autorisasjonen din. Ikke skriv inn autorisasjonsnummeret ditt." ]
                    ]

        Oppsummering oppsummeringsType validertSkjema ->
            case oppsummeringsType of
                AvbrøtSletting ->
                    case Skjema.fagdokumentasjonType (Skjema.tilUvalidertSkjema validertSkjema) of
                        SvennebrevFagbrev ->
                            [ Melding.spørsmål [ "Ok, da lar jeg fagbrevet/svennebrevet stå." ]
                            , oppsummeringsSpørsmål validertSkjema
                            ]

                        Mesterbrev ->
                            [ Melding.spørsmål [ "Ok, da lar jeg mesterbrevet stå." ]
                            , oppsummeringsSpørsmål validertSkjema
                            ]

                        Autorisasjon ->
                            [ Melding.spørsmål [ "Ok, da lar jeg autorisasjonen stå." ]
                            , oppsummeringsSpørsmål validertSkjema
                            ]

                EtterEndring ->
                    [ Melding.spørsmål [ "Du har endret. Er det riktig nå?" ] ]

                FørsteGang ->
                    [ oppsummeringsSpørsmål validertSkjema
                    ]

        EndrerOppsummering _ skjema ->
            case Skjema.fagdokumentasjonType skjema of
                SvennebrevFagbrev ->
                    [ Melding.spørsmål [ "Gå gjennom og endre det du ønsker." ] ]

                Mesterbrev ->
                    [ Melding.spørsmål [ "Nå kan du endre i feltene under." ] ]

                Autorisasjon ->
                    [ Melding.spørsmål [ "Gjør endringene du ønsker." ] ]

        BekreftSlettingAvPåbegynt skjema ->
            case Skjema.fagdokumentasjonType (Skjema.tilUvalidertSkjema skjema) of
                SvennebrevFagbrev ->
                    [ Melding.spørsmål [ "Er du sikker på at du vil slette dette fagbrevet/svennebrevet?" ] ]

                Mesterbrev ->
                    [ Melding.spørsmål [ "Er du sikker på at du vil slette dette mesterbrevet?" ] ]

                Autorisasjon ->
                    [ Melding.spørsmål [ "Er du sikker på at du vil slette denne autorisasjonen?" ] ]

        --todo: legg inn
        LagringFeilet validertSkjema error ->
            [ ErrorHåndtering.errorMelding { operasjon = lagreOperasjonStringFraSkjema validertSkjema, error = error } ]

        BekreftAvbrytingAvRegistreringen _ ->
            case fagdokumentasjonType of
                SvennebrevFagbrev ->
                    [ Melding.spørsmål [ "Hvis du avbryter, blir ikke fagbrevet/svennebrevet lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

                Mesterbrev ->
                    [ Melding.spørsmål [ "Hvis du avbryter, blir ikke mesterbrevet lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

                Autorisasjon ->
                    [ Melding.spørsmål [ "Hvis du avbryter, blir ikke autorisasjonen lagret på CV-en din. Er du sikker på at du vil avbryte?" ] ]

        VenterPåAnimasjonFørFullføring _ avsluttetGrunn ->
            case avsluttetGrunn of
                AvbruttPåbegynt ->
                    [ Melding.spørsmål [ "Nå har jeg avbrutt. Vil du legge inn flere kategorier?" ] ]

                SlettetPåbegynt ->
                    case fagdokumentasjonType of
                        SvennebrevFagbrev ->
                            [ Melding.spørsmål [ "Nå har jeg slettet fagbrevet/svennebrevet. Vil du legge inn flere kategorier?" ] ]

                        Mesterbrev ->
                            [ Melding.spørsmål [ "Nå har jeg slettet mesterbrevet. Vil du legge inn flere kategorier?" ] ]

                        Autorisasjon ->
                            [ Melding.spørsmål [ "Nå har jeg slettet autorisasjonen. Vil du legge inn flere kategorier?" ] ]

                AnnenAvslutning ->
                    [ Melding.spørsmål [ "Vil du legge inn flere kategorier?" ] ]

        Lagrer _ _ ->
            []


lagreOperasjonStringFraSkjema : ValidertFagdokumentasjonSkjema -> String
lagreOperasjonStringFraSkjema skjema =
    skjema
        |> Skjema.tilUvalidertSkjema
        |> Skjema.fagdokumentasjonType
        |> lagreOperasjonStringFraFagdokumentasjonType


lagreOperasjonStringFraFagdokumentasjonType : FagdokumentasjonType -> String
lagreOperasjonStringFraFagdokumentasjonType fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            "lagre fagbrevet/svennebrevet"

        Mesterbrev ->
            "lagre mesterbrevet"

        Autorisasjon ->
            "lagre autorisasjonen"


settFokus : Samtale -> Cmd Msg
settFokus samtale =
    case samtale of
        RegistrerKonsept _ _ ->
            settFokusCmd RegistrerKonseptInput

        RegistrerBeskrivelse _ _ ->
            settFokusCmd RegistrerBeskrivelseInput

        Oppsummering _ _ ->
            settFokusCmd BekreftOppsummeringId

        EndrerOppsummering _ _ ->
            settFokusCmd RegistrerKonseptInput

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


oppsummeringsSpørsmål : ValidertFagdokumentasjonSkjema -> Melding
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


eksemplerPåFagdokumentasjon : FagdokumentasjonType -> List Melding
eksemplerPåFagdokumentasjon fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            [ Melding.eksempel [ "Tok svennebrev som tømrer i 2017 etter lærlingtid hos Byggmester Hansen." ]
            ]

        Mesterbrev ->
            [ Melding.eksempel [ "Tok mesterbrev som herrefrisør i 1994." ]
            ]

        Autorisasjon ->
            [ Melding.eksempel [ "Har norsk autorisasjon som sykepleier fra 2012." ]
            ]



--- VIEW ---


type InputId
    = RegistrerKonseptInput
    | RegistrerBeskrivelseInput
    | BekreftOppsummeringId
    | SlettePåbegyntId
    | LagringFeiletActionId
    | AvbrytSlettingId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        RegistrerKonseptInput ->
            "fagdokumentasjon-registrer-konsept"

        RegistrerBeskrivelseInput ->
            "fagdokumentasjon-registrer-beskrivelse"

        BekreftOppsummeringId ->
            "fagdokumentasjon-bekreft-oppsummering-id"

        SlettePåbegyntId ->
            "fagdokumentasjon-slett-påbegynt-id"

        LagringFeiletActionId ->
            "fagdokumentasjon-lagring-feilet-id"

        AvbrytSlettingId ->
            "fagdokumentasjon-avbrytt-slett-id"


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    model
        |> modelTilBrukerInput
        |> BrukerInput.toHtml


modelTilBrukerInput : ModelInfo -> BrukerInput Msg
modelTilBrukerInput model =
    if MeldingsLogg.visBrukerInput model.seksjonsMeldingsLogg then
        case model.aktivSamtale of
            RegistrerKonsept visFeilmelding typeaheadModel ->
                BrukerInput.typeaheadMedGåVidereKnapp { onAvbryt = BrukerVilAvbryteRegistreringen, onGåVidere = BrukerVilRegistrereKonsept }
                    (typeaheadModel
                        |> feilmeldingTypeahead model.fagdokumentasjonType
                        |> maybeHvisTrue visFeilmelding
                        |> Typeahead.toViewElement Konsept.label typeaheadModel
                        |> FrontendModuler.Typeahead.map TypeaheadMsg
                    )

            RegistrerBeskrivelse medEksempelKnapp beskrivelseinfo ->
                beskrivelseinfo.beskrivelse
                    |> Textarea.textarea { msg = OppdaterFagdokumentasjonBeskrivelse, label = "Kort beskrivelse" }
                    |> Textarea.withFeilmelding (feilmeldingBeskrivelsesfelt beskrivelseinfo.beskrivelse)
                    |> Textarea.withId (inputIdTilString RegistrerBeskrivelseInput)
                    |> BrukerInputMedGåVidereKnapp.textarea BrukerVilRegistrereFagdokumentasjonBeskrivelse
                    |> BrukerInputMedGåVidereKnapp.withVisEksempelKnapp medEksempelKnapp VilSeEksempel
                    |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp BrukerVilAvbryteRegistreringen
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            Oppsummering _ _ ->
                viewBekreftOppsummering

            EndrerOppsummering typeaheadModel skjema ->
                BrukerInput.skjema { lagreMsg = BrukerLagrerSkjema, lagreKnappTekst = "Lagre endringer" }
                    [ skjema
                        |> Skjema.feilmeldingTypeahead
                        |> Typeahead.view Konsept.label typeaheadModel
                        |> Html.map TypeaheadMsg
                    , skjema
                        |> Skjema.beskrivelse
                        |> Textarea.textarea { label = "Beskrivelse", msg = OppdaterFagdokumentasjonBeskrivelse }
                        |> Textarea.withFeilmelding (Skjema.beskrivelse skjema |> feilmeldingBeskrivelsesfelt)
                        |> Textarea.toHtml
                    ]

            BekreftSlettingAvPåbegynt _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BekrefterSlettPåbegynt "Ja, jeg vil slette"
                        |> Knapp.withId (inputIdTilString SlettePåbegyntId)
                    , Knapp.knapp AngrerSlettPåbegynt "Nei, jeg vil ikke slette"
                    ]

            Lagrer _ _ ->
                BrukerInput.utenInnhold

            LagringFeilet _ error ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilIkkePrøveÅLagrePåNytt "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagrePåNytt "Prøv på nytt"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp BrukerVilIkkePrøveÅLagrePåNytt "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            BekreftAvbrytingAvRegistreringen _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerBekrefterAvbrytingAvRegistrering "Ja, jeg vil avbryte"
                        |> Knapp.withId (inputIdTilString AvbrytSlettingId)
                    , Knapp.knapp BrukerVilIkkeAvbryteRegistreringen "Nei, jeg vil fortsette"
                    ]

            VenterPåAnimasjonFørFullføring _ _ ->
                BrukerInput.utenInnhold

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


feilmeldingTypeahead : FagdokumentasjonType -> Typeahead.Model Konsept -> Maybe String
feilmeldingTypeahead fagdokumentasjonType typeaheadModel =
    case Typeahead.selected typeaheadModel of
        Just _ ->
            Nothing

        Nothing ->
            Just (feilmeldingstekstIkkeValgtKonsept fagdokumentasjonType)


maybeHvisTrue : Bool -> Maybe a -> Maybe a
maybeHvisTrue bool maybe =
    if bool then
        maybe

    else
        Nothing


feilmeldingBeskrivelsesfelt : String -> Maybe String
feilmeldingBeskrivelsesfelt innhold =
    if String.length innhold <= 200 then
        Nothing

    else
        let
            tallTekst =
                (String.length innhold - 200)
                    |> String.fromInt
        in
        Just ("Du har " ++ tallTekst ++ " tegn for mye")


typeaheadLabel : FagdokumentasjonType -> String
typeaheadLabel fagdokumentasjonType =
    case fagdokumentasjonType of
        SvennebrevFagbrev ->
            "Fagbrev/svennebrev"

        Mesterbrev ->
            "Mesterbrev"

        Autorisasjon ->
            "Autorisasjon"



--- INIT ---


init : FagdokumentasjonType -> DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
init fagdokumentasjonType debugStatus gammelMeldingsLogg fagdokumentasjonListe =
    let
        aktivSamtale =
            initSamtaleTypeahead fagdokumentasjonType
                |> Tuple.first
                |> RegistrerKonsept False
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg fagdokumentasjonType aktivSamtale)
        , aktivSamtale = aktivSamtale
        , fagdokumentasjonListe = fagdokumentasjonListe
        , fagdokumentasjonType = fagdokumentasjonType
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )


initFagbrev : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initFagbrev =
    init SvennebrevFagbrev


initMesterbrev : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initMesterbrev =
    init Mesterbrev


initAutorisasjon : DebugStatus -> FerdigAnimertMeldingsLogg -> List Fagdokumentasjon -> ( Model, Cmd Msg )
initAutorisasjon =
    init Autorisasjon


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ Browser.Events.onVisibilityChange WindowEndrerVisibility
        , model.seksjonsMeldingsLogg
            |> SamtaleAnimasjon.subscriptions
            |> Sub.map SamtaleAnimasjonMsg
        ]
