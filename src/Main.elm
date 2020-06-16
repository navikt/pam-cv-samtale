module Main exposing (main, viewMeldingsLogg)

import AnnenErfaring.Seksjon
import Api
import Arbeidserfaring.Seksjon
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Browser.Navigation as Navigation
import Cv exposing (Cv)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Fagdokumentasjon.Seksjon
import Feilmelding
import Forerkort.Seksjon
import FrontendModuler.Alertstripe as Alertstripe exposing (..)
import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput, KnapperLayout(..))
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp
import FrontendModuler.Header as Header
import FrontendModuler.Knapp as Knapp exposing (Enabled(..), Knapp)
import FrontendModuler.Lenke as Lenke
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Spinner as Spinner
import FrontendModuler.Textarea as Textarea exposing (Textarea)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel, ariaLive)
import Http
import Jobbprofil.Seksjon
import Kurs.Seksjon
import LagreStatus exposing (LagreStatus)
import Meldinger.Konstanter as Konstanter
import Meldinger.Melding as Melding exposing (Melding, MeldingsType(..), Tekstområde(..))
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsGruppeViewState(..), MeldingsLogg, SpørsmålsGruppeViewState)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Meldinger.SporsmalViewState as SpørsmålViewState exposing (IkonStatus(..), SpørsmålStyle(..), SpørsmålViewState)
import Metrikker
import Person exposing (BrukerInfo(..), Person, Synlighet(..))
import Personalia.Personalia as Personalia exposing (Personalia)
import Personalia.Seksjon
import Process
import Sammendrag exposing (Sammendrag)
import Sertifikat.Seksjon
import Sprak.Seksjon
import Task
import Tid exposing (tilKlokkeSlett, ulikDato)
import TilbakemeldingModal
import Time exposing (Posix)
import Url
import Utdanning.Seksjon
import Validering



--- RegistreringsProgresjon ---


type alias RegistreringsProgresjon =
    { erDetteFørsteGangManErInneILøsningen : Bool
    , personalia : Seksjonsstatus
    , utdanning : Seksjonsstatus
    }


type Seksjonsstatus
    = IkkeBegynt
    | Begynt
    | Fullført



--- MODEL ---


type alias ExtendedModel =
    { model : Model
    , navigationKey : Navigation.Key
    , debugStatus : DebugStatus
    , windowWidth : Int
    , modalStatus : ModalStatus
    , timeZone : Time.Zone
    , time : Time.Posix
    }


type ModalStatus
    = ModalLukket
    | TilbakemeldingModalÅpen TilbakemeldingModal.Model


type Model
    = Loading LoadingModel
    | Success SuccessModel
    | Failure Http.Error



--- UPDATE ---


type Msg
    = LoadingMsg LoadingMsg
    | SuccessMsg SuccessMsg
    | ErrorLoggetUnderLoading (Result Http.Error ())
    | ViewportHentet Dom.Viewport
    | WindowResized Int Int
    | UrlChanged Url.Url
    | UrlRequestChanged Browser.UrlRequest
    | ÅpneTilbakemeldingModal
    | ModalMsg TilbakemeldingModal.Msg
    | Tick Posix
    | AdjustTimeZone Time.Zone


update : Msg -> ExtendedModel -> ( ExtendedModel, Cmd Msg )
update msg extendedModel =
    case msg of
        LoadingMsg loadingModel ->
            updateLoading extendedModel.debugStatus loadingModel extendedModel.model
                |> mapTilExtendedModel extendedModel

        SuccessMsg successMsg ->
            case extendedModel.model of
                Success successModel ->
                    let
                        ( nyModel, successCmd ) =
                            updateSuccess successMsg successModel
                    in
                    ( Success nyModel, Cmd.map SuccessMsg successCmd )
                        |> mapTilExtendedModel extendedModel

                _ ->
                    ( extendedModel, Cmd.none )

        ErrorLoggetUnderLoading _ ->
            ( extendedModel, Cmd.none )

        WindowResized windowWidth _ ->
            ( { model = extendedModel.model
              , navigationKey = extendedModel.navigationKey
              , debugStatus = extendedModel.debugStatus
              , modalStatus = extendedModel.modalStatus
              , windowWidth = windowWidth
              , timeZone = extendedModel.timeZone
              , time = extendedModel.time
              }
            , Cmd.none
            )

        UrlChanged _ ->
            ( extendedModel, Cmd.none )

        UrlRequestChanged (Browser.External urlString) ->
            ( extendedModel, Navigation.load urlString )

        UrlRequestChanged (Browser.Internal url) ->
            ( extendedModel, Navigation.load (Url.toString url) )

        ViewportHentet viewport ->
            ( { model = extendedModel.model
              , navigationKey = extendedModel.navigationKey
              , debugStatus = extendedModel.debugStatus
              , modalStatus = extendedModel.modalStatus
              , windowWidth = round viewport.scene.width
              , timeZone = extendedModel.timeZone
              , time = extendedModel.time
              }
            , Cmd.none
            )

        ÅpneTilbakemeldingModal ->
            let
                ( modalModel, cmd ) =
                    extendedModel.model
                        |> modelTilMetrikkSeksjon
                        |> TilbakemeldingModal.init
            in
            ( { extendedModel | modalStatus = TilbakemeldingModalÅpen modalModel }
            , Cmd.map ModalMsg cmd
            )

        ModalMsg modalMsg ->
            case extendedModel.modalStatus of
                ModalLukket ->
                    ( extendedModel, Cmd.none )

                TilbakemeldingModalÅpen model ->
                    case TilbakemeldingModal.update modalMsg model of
                        TilbakemeldingModal.Open nyModel cmd ->
                            ( { extendedModel | modalStatus = TilbakemeldingModalÅpen nyModel }
                            , Cmd.map ModalMsg cmd
                            )

                        TilbakemeldingModal.Closed ->
                            ( { extendedModel | modalStatus = ModalLukket }, Cmd.none )

        Tick newTime ->
            ( { extendedModel | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { extendedModel | timeZone = newZone }
            , Cmd.none
            )


mapTilExtendedModel : ExtendedModel -> ( Model, Cmd Msg ) -> ( ExtendedModel, Cmd Msg )
mapTilExtendedModel extendedModel ( model, cmd ) =
    ( { model = model
      , windowWidth = extendedModel.windowWidth
      , debugStatus = extendedModel.debugStatus
      , navigationKey = extendedModel.navigationKey
      , modalStatus = extendedModel.modalStatus
      , timeZone = extendedModel.timeZone
      , time = extendedModel.time
      }
    , cmd
    )


modelTilMetrikkSeksjon : Model -> Metrikker.Seksjon
modelTilMetrikkSeksjon model =
    case model of
        Loading _ ->
            Metrikker.Loading

        Failure _ ->
            Metrikker.Failure

        Success successModel ->
            successModelTilMetrikkSeksjon successModel



--- LOADING MODEL ---


type LoadingModel
    = VenterPåPerson
    | VenterPåPersonalia Person
    | VenterPåResten LoadingState


type alias LoadingState =
    { cv : Maybe Cv
    , personalia : Personalia
    , person : Person
    , registreringsProgresjon : Maybe RegistreringsProgresjon
    , windowWidth : Maybe Int
    }



--- LOADING UPDATE ---


type LoadingMsg
    = PersonHentet (Result Http.Error Person)
    | PersonOpprettet (Result Http.Error Person)
    | PersonaliaHentet (Result Http.Error Personalia)
    | PersonaliaOpprettet (Result Http.Error Personalia)
    | CvHentet (Result Http.Error Cv)
    | CvOpprettet (Result Http.Error Cv)
    | RegistreringsProgresjonHentet (Result Http.Error RegistreringsProgresjon)


updateLoading : DebugStatus -> LoadingMsg -> Model -> ( Model, Cmd Msg )
updateLoading debugStatus msg model =
    case msg of
        PersonHentet result ->
            case result of
                Ok person ->
                    personHentet model person

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            ( model, Api.opprettPerson (PersonOpprettet >> LoadingMsg) )

                        _ ->
                            uhåndtertErrorUnderLoading model error "Hent Person"

        PersonOpprettet result ->
            case result of
                Ok person ->
                    personHentet model person

                Err error ->
                    uhåndtertErrorUnderLoading model error "Opprett Person"

        PersonaliaHentet result ->
            case model of
                Loading (VenterPåPersonalia person) ->
                    case result of
                        Ok personalia ->
                            initVenterPåResten person personalia

                        Err error ->
                            case error of
                                Http.BadStatus 404 ->
                                    ( model, Api.opprettPersonalia (PersonaliaOpprettet >> LoadingMsg) )

                                _ ->
                                    uhåndtertErrorUnderLoading model error "Hent Personalia"

                _ ->
                    ( model, Cmd.none )

        PersonaliaOpprettet result ->
            case model of
                Loading (VenterPåPersonalia person) ->
                    case result of
                        Ok personalia ->
                            initVenterPåResten person personalia

                        Err error ->
                            uhåndtertErrorUnderLoading model error "Opprett Personalia"

                _ ->
                    ( model, Cmd.none )

        CvHentet result ->
            case model of
                Loading (VenterPåResten state) ->
                    case result of
                        Ok cv ->
                            modelFraLoadingState debugStatus { state | cv = Just cv }

                        Err error ->
                            case error of
                                Http.BadStatus 404 ->
                                    ( model, Api.opprettCv (CvOpprettet >> LoadingMsg) )

                                _ ->
                                    uhåndtertErrorUnderLoading model error "Hent CV"

                _ ->
                    ( model, Cmd.none )

        CvOpprettet result ->
            case model of
                Loading (VenterPåResten state) ->
                    case result of
                        Ok cv ->
                            modelFraLoadingState debugStatus { state | cv = Just cv }

                        Err error ->
                            uhåndtertErrorUnderLoading model error "Opprett CV"

                _ ->
                    ( model, Cmd.none )

        RegistreringsProgresjonHentet result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err error ->
                    uhåndtertErrorUnderLoading model error "Hent registreringsprogresjon"


personHentet : Model -> Person -> ( Model, Cmd Msg )
personHentet model person =
    if Person.harGodtattVilkår person then
        ( Loading (VenterPåPersonalia person)
        , Api.getPersonalia (PersonaliaHentet >> LoadingMsg)
        )

    else
        ( model, redirectTilGodkjenningAvSamtykke )


redirectTilLogin : Cmd msg
redirectTilLogin =
    Navigation.load "/cv-samtale/login"


uhåndtertErrorUnderLoading : Model -> Http.Error -> String -> ( Model, Cmd Msg )
uhåndtertErrorUnderLoading model error operasjon =
    case error of
        Http.BadStatus 401 ->
            ( model, redirectTilLogin )

        _ ->
            ( Failure error, logFeilmeldingUnderLoading error operasjon )


redirectTilGodkjenningAvSamtykke : Cmd Msg
redirectTilGodkjenningAvSamtykke =
    Navigation.load "/cv/samtykke-for-cv-samtale"


logFeilmeldingUnderLoading : Http.Error -> String -> Cmd Msg
logFeilmeldingUnderLoading error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLoggetUnderLoading)
        |> Maybe.withDefault Cmd.none


modelFraLoadingState : DebugStatus -> LoadingState -> ( Model, Cmd Msg )
modelFraLoadingState debugStatus state =
    case ( state.cv, state.registreringsProgresjon ) of
        ( Just cv, Just registreringsProgresjon ) ->
            ( Success
                { cv = cv
                , personalia = state.personalia
                , person = state.person
                , registreringsProgresjon = registreringsProgresjon
                , aktivSeksjon = initialiserSamtale (Cv.sistEndretDato cv) state.personalia
                , debugStatus = debugStatus
                }
            , lagtTilSpørsmålCmd debugStatus
                |> Cmd.map SuccessMsg
            )

        _ ->
            ( Loading (VenterPåResten state), Cmd.none )


initialiserSamtale : Posix -> Personalia -> SamtaleSeksjon
initialiserSamtale sistLagretFraCv personalia =
    let
        aktivSamtale =
            Introduksjon personalia
    in
    AndreSamtaleSteg
        { aktivSamtale = aktivSamtale
        , meldingsLogg =
            MeldingsLogg.init
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , sistLagret = sistLagretFraCv
        }


initVenterPåResten : Person -> Personalia -> ( Model, Cmd Msg )
initVenterPåResten person personalia =
    ( Loading
        (VenterPåResten
            { cv = Nothing
            , personalia = personalia
            , person = person
            , windowWidth = Nothing
            , registreringsProgresjon =
                Just
                    { erDetteFørsteGangManErInneILøsningen = True
                    , personalia = IkkeBegynt
                    , utdanning = IkkeBegynt
                    }
            }
        )
    , Api.getCv (CvHentet >> LoadingMsg)
    )



--- SUCCESS MODEL ---


type alias SuccessModel =
    { cv : Cv
    , personalia : Personalia
    , person : Person
    , registreringsProgresjon : RegistreringsProgresjon
    , aktivSeksjon : SamtaleSeksjon
    , debugStatus : DebugStatus
    }


type SamtaleSeksjon
    = PersonaliaSeksjon Personalia.Seksjon.Model
    | UtdanningSeksjon Utdanning.Seksjon.Model
    | ArbeidsErfaringSeksjon Arbeidserfaring.Seksjon.Model
    | SpråkSeksjon Sprak.Seksjon.Model
    | FagdokumentasjonSeksjon Fagdokumentasjon.Seksjon.Model
    | SertifikatSeksjon Sertifikat.Seksjon.Model
    | AnnenErfaringSeksjon AnnenErfaring.Seksjon.Model
    | FørerkortSeksjon Forerkort.Seksjon.Model
    | KursSeksjon Kurs.Seksjon.Model
    | AndreSamtaleSteg AndreSamtaleStegInfo
    | JobbprofilSeksjon Jobbprofil.Seksjon.Model



--- SUCCESS UPDATE ---


type SuccessMsg
    = PersonaliaMsg Personalia.Seksjon.Msg
    | UtdanningsMsg Utdanning.Seksjon.Msg
    | ArbeidserfaringsMsg Arbeidserfaring.Seksjon.Msg
    | SpråkMsg Sprak.Seksjon.Msg
    | FagdokumentasjonMsg Fagdokumentasjon.Seksjon.Msg
    | SertifikatMsg Sertifikat.Seksjon.Msg
    | AnnenErfaringMsg AnnenErfaring.Seksjon.Msg
    | FørerkortMsg Forerkort.Seksjon.Msg
    | KursMsg Kurs.Seksjon.Msg
    | AndreSamtaleStegMsg AndreSamtaleStegMsg
    | JobbprofilMsg Jobbprofil.Seksjon.Msg
    | FokusSatt (Result Dom.Error ())


updateSuccess : SuccessMsg -> SuccessModel -> ( SuccessModel, Cmd SuccessMsg )
updateSuccess successMsg model =
    case successMsg of
        PersonaliaMsg msg ->
            case model.aktivSeksjon of
                PersonaliaSeksjon personaliaModel ->
                    case Personalia.Seksjon.update msg personaliaModel of
                        Personalia.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> PersonaliaSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map PersonaliaMsg cmd
                            )

                        Personalia.Seksjon.Ferdig sistLagret _ personaliaMeldingsLogg ->
                            gåTilUtdanning sistLagret model personaliaMeldingsLogg

                _ ->
                    ( model, Cmd.none )

        UtdanningsMsg msg ->
            case model.aktivSeksjon of
                UtdanningSeksjon utdanningModel ->
                    case Utdanning.Seksjon.update msg utdanningModel of
                        Utdanning.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> UtdanningSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map UtdanningsMsg cmd
                            )

                        Utdanning.Seksjon.Ferdig sistLagret _ meldingsLogg ->
                            gåTilArbeidserfaring sistLagret model meldingsLogg

                _ ->
                    ( model, Cmd.none )

        ArbeidserfaringsMsg msg ->
            case model.aktivSeksjon of
                ArbeidsErfaringSeksjon arbeidserfaringsModel ->
                    case Arbeidserfaring.Seksjon.update msg arbeidserfaringsModel of
                        Arbeidserfaring.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> ArbeidsErfaringSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map ArbeidserfaringsMsg cmd
                            )

                        Arbeidserfaring.Seksjon.Ferdig sistLagret ferdigAnimertMeldingsLogg ->
                            gåTilSpråk sistLagret model ferdigAnimertMeldingsLogg

                _ ->
                    ( model, Cmd.none )

        SpråkMsg msg ->
            case model.aktivSeksjon of
                SpråkSeksjon språkModel ->
                    case Sprak.Seksjon.update msg språkModel of
                        Sprak.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SpråkSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map SpråkMsg cmd
                            )

                        Sprak.Seksjon.Ferdig sistLagret meldingsLogg ->
                            gåTilFørerkort sistLagret model meldingsLogg

                _ ->
                    ( model, Cmd.none )

        FagdokumentasjonMsg msg ->
            case model.aktivSeksjon of
                FagdokumentasjonSeksjon fagdokumentasjonModel ->
                    case Fagdokumentasjon.Seksjon.update msg fagdokumentasjonModel of
                        Fagdokumentasjon.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> FagdokumentasjonSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map FagdokumentasjonMsg cmd
                            )

                        Fagdokumentasjon.Seksjon.Ferdig sistLagret fagdokumentasjonListe meldingsLogg ->
                            gåTilFlereSeksjonsValg sistLagret model meldingsLogg

                _ ->
                    ( model, Cmd.none )

        SertifikatMsg msg ->
            case model.aktivSeksjon of
                SertifikatSeksjon sertifikatModel ->
                    case Sertifikat.Seksjon.update msg sertifikatModel of
                        Sertifikat.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SertifikatSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map SertifikatMsg cmd
                            )

                        Sertifikat.Seksjon.Ferdig sistLagret sertifikatListe meldingsLogg ->
                            gåTilFlereAnnetValg sistLagret model meldingsLogg

                _ ->
                    ( model, Cmd.none )

        AnnenErfaringMsg msg ->
            case model.aktivSeksjon of
                AnnenErfaringSeksjon annenErfaringModel ->
                    case AnnenErfaring.Seksjon.update msg annenErfaringModel of
                        AnnenErfaring.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> AnnenErfaringSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map AnnenErfaringMsg cmd
                            )

                        AnnenErfaring.Seksjon.Ferdig sistLagret _ meldingsLogg ->
                            gåTilFlereAnnetValg sistLagret model meldingsLogg

                _ ->
                    ( model, Cmd.none )

        FørerkortMsg msg ->
            case model.aktivSeksjon of
                FørerkortSeksjon førerkortModel ->
                    case Forerkort.Seksjon.update msg førerkortModel of
                        Forerkort.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> FørerkortSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map FørerkortMsg cmd
                            )

                        Forerkort.Seksjon.Ferdig sistLagret førerkort meldingsLogg ->
                            gåTilSeksjonsValg sistLagret { model | cv = Cv.oppdaterFørerkort førerkort model.cv } meldingsLogg

                _ ->
                    ( model, Cmd.none )

        KursMsg msg ->
            case model.aktivSeksjon of
                KursSeksjon kursModel ->
                    case Kurs.Seksjon.update msg kursModel of
                        Kurs.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> KursSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map KursMsg cmd
                            )

                        Kurs.Seksjon.Ferdig sistLagret _ meldingsLogg ->
                            gåTilFlereAnnetValg sistLagret model meldingsLogg

                _ ->
                    ( model, Cmd.none )

        AndreSamtaleStegMsg andreSamtaleStegMsg ->
            case model.aktivSeksjon of
                AndreSamtaleSteg andreSamtaleStegInfo ->
                    updateAndreSamtaleSteg model andreSamtaleStegMsg andreSamtaleStegInfo

                _ ->
                    ( model, Cmd.none )

        JobbprofilMsg msg ->
            case model.aktivSeksjon of
                JobbprofilSeksjon jobbprofilModel ->
                    case Jobbprofil.Seksjon.update msg jobbprofilModel of
                        Jobbprofil.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> JobbprofilSeksjon
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.map JobbprofilMsg cmd
                            )

                        Jobbprofil.Seksjon.Ferdig sistLagret brukerInfo ferdigAnimertMeldingsLogg ->
                            ( { aktivSamtale = InformerOmEures True
                              , meldingsLogg =
                                    ferdigAnimertMeldingsLogg
                                        |> MeldingsLogg.tilMeldingsLogg
                                        |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg (InformerOmEures True))
                              , sistLagret = sistLagret
                              }
                                |> AndreSamtaleSteg
                                |> oppdaterSamtaleSeksjon model
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                _ ->
                    ( model, Cmd.none )

        FokusSatt _ ->
            ( model, Cmd.none )


oppdaterSamtaleSeksjon : SuccessModel -> SamtaleSeksjon -> SuccessModel
oppdaterSamtaleSeksjon model samtaleSeksjon =
    { model | aktivSeksjon = samtaleSeksjon }


gåTilArbeidserfaring : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilArbeidserfaring sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( arbeidsModell, arbeidsCmd ) =
            Arbeidserfaring.Seksjon.init model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.arbeidserfaring model.cv)
    in
    ( { model | aktivSeksjon = ArbeidsErfaringSeksjon arbeidsModell }
    , Cmd.map ArbeidserfaringsMsg arbeidsCmd
    )


gåTilUtdanning : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilUtdanning sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( utdanningModel, utdanningCmd ) =
            Utdanning.Seksjon.init model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.utdanning model.cv)
    in
    ( { model
        | aktivSeksjon = UtdanningSeksjon utdanningModel
      }
    , Cmd.map UtdanningsMsg utdanningCmd
    )


gåTilSpråk : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilSpråk sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( språkModel, språkCmd ) =
            Sprak.Seksjon.init model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.spraakferdighet model.cv)
    in
    ( { model
        | aktivSeksjon = SpråkSeksjon språkModel
      }
    , Cmd.map SpråkMsg språkCmd
    )


gåTilFagbrev : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilFagbrev sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initFagbrev model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( { model
        | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
      }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilMesterbrev : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilMesterbrev sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initMesterbrev model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( { model
        | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
      }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilSertifisering : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilSertifisering sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( sertifikatModel, sertifikatCmd ) =
            Sertifikat.Seksjon.init model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.sertifikater model.cv)
    in
    ( { model
        | aktivSeksjon = SertifikatSeksjon sertifikatModel
      }
    , Cmd.map SertifikatMsg sertifikatCmd
    )


gåTilAnnenErfaring : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilAnnenErfaring sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( annenErfaringModel, annenErfaringCmd ) =
            AnnenErfaring.Seksjon.init model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.annenErfaring model.cv)
    in
    ( { model
        | aktivSeksjon = AnnenErfaringSeksjon annenErfaringModel
      }
    , Cmd.map AnnenErfaringMsg annenErfaringCmd
    )


gåTilKurs : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilKurs sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( kursModel, kursCmd ) =
            Kurs.Seksjon.init model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.kurs model.cv)
    in
    ( { model
        | aktivSeksjon = KursSeksjon kursModel
      }
    , Cmd.map KursMsg kursCmd
    )


gåTilFørerkort : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilFørerkort sistLagret model ferdigAnimertMeldingsLogg =
    let
        ( førerkortModel, førerkortCmd ) =
            Forerkort.Seksjon.init model.debugStatus sistLagret ferdigAnimertMeldingsLogg (Cv.førerkort model.cv)
    in
    ( { model
        | aktivSeksjon = FørerkortSeksjon førerkortModel
      }
    , Cmd.map FørerkortMsg førerkortCmd
    )


gåTilSeksjonsValg : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilSeksjonsValg sistLagret model ferdigAnimertMeldingsLogg =
    ( { aktivSamtale = LeggTilAutorisasjoner
      , meldingsLogg =
            ferdigAnimertMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg LeggTilAutorisasjoner)
      , sistLagret = sistLagret
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )


gåTilFlereSeksjonsValg : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilFlereSeksjonsValg sistLagret model ferdigAnimertMeldingsLogg =
    ( { aktivSamtale = LeggTilFlereAutorisasjoner
      , meldingsLogg =
            ferdigAnimertMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg LeggTilFlereAutorisasjoner)
      , sistLagret = sistLagret
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )


gåTilFlereAnnetValg : Posix -> SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilFlereAnnetValg sistLagret model ferdigAnimertMeldingsLogg =
    ( { aktivSamtale = LeggTilFlereAnnet
      , meldingsLogg =
            ferdigAnimertMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg LeggTilFlereAnnet)
      , sistLagret = sistLagret
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )


navigerTilEures : Samtale -> Samtale
navigerTilEures msg =
    Navigation.load "https://ec.europa.eu/eures/public/no/homepage"
        |> (\_ -> msg)


successModelTilMetrikkSeksjon : SuccessModel -> Metrikker.Seksjon
successModelTilMetrikkSeksjon { aktivSeksjon } =
    case aktivSeksjon of
        PersonaliaSeksjon _ ->
            Metrikker.Personalia

        UtdanningSeksjon _ ->
            Metrikker.Utdanning

        ArbeidsErfaringSeksjon _ ->
            Metrikker.Arbeidserfaring

        SpråkSeksjon _ ->
            Metrikker.Språk

        FagdokumentasjonSeksjon _ ->
            Metrikker.Fagdokumentasjon

        SertifikatSeksjon _ ->
            Metrikker.Sertifikat

        AnnenErfaringSeksjon _ ->
            Metrikker.AnnenErfaring

        FørerkortSeksjon _ ->
            Metrikker.Førerkort

        KursSeksjon _ ->
            Metrikker.Kurs

        AndreSamtaleSteg andreSamtaleStegInfo ->
            andreSamtaleStegTilMetrikkSeksjon andreSamtaleStegInfo

        JobbprofilSeksjon _ ->
            Metrikker.Jobbprofil



--- ANDRE SAMTALESTEG MODEL ---


type alias AndreSamtaleStegInfo =
    { aktivSamtale : Samtale
    , meldingsLogg : MeldingsLogg
    , sistLagret : Posix
    }


type BekreftSammendragState
    = OpprinneligSammendrag Sammendrag
    | NyttSammendrag String
    | EndretSammendrag String


type Samtale
    = Introduksjon Personalia
    | LeggTilAutorisasjoner
    | LeggTilFlereAutorisasjoner
    | LeggTilAnnet
    | LeggTilFlereAnnet
    | BekreftSammendrag Bool BekreftSammendragState
    | SkriverSammendrag Bool String
    | EndrerSammendrag Bool String
    | LagrerSammendrag String LagreStatus
    | LagringAvSammendragFeilet Http.Error String
    | DelMedArbeidsgiver
    | VenterPåÅGåTilJobbprofil BrukerInfo
    | LagrerSynlighet Bool LagreStatus
    | LagringSynlighetFeilet Http.Error Bool
    | InformerOmEures Bool
    | SpørOmTilbakemelding
    | Avslutt Bool



--- ANDRE SAMTALESTEG UPDATE ---


type AndreSamtaleStegMsg
    = BrukerSierHeiIIntroduksjonen
    | VilSeEksempel
    | BrukerVilEndreSammendrag
    | SammendragEndret String
    | VilLagreSammendragSkjema
    | VilLagreBekreftetSammendrag
    | VilIkkeLagreSammendrag
    | SammendragOppdatert (Result Http.Error Sammendrag)
    | SeksjonValgt ValgtSeksjon
    | IngenAvAutorisasjonSeksjoneneValgt
    | IngenAvDeAndreSeksjoneneValgt
    | BrukerGodkjennerSynligCV
    | BrukerGodkjennerIkkeSynligCV
    | BrukerVilPrøveÅLagreSynlighetPåNytt
    | BrukerGirOppÅLagreSynlighet Bool
    | HarSvartJaTilEures
    | HarSvartNeiTilEures
    | VilGiTilbakemelding
    | VilIkkeGiTilbakemelding
    | SynlighetPostet (Result Http.Error Bool)
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | ErrorLogget


type ValgtSeksjon
    = FagbrevSvennebrevValgt
    | MesterbrevValgt
      --| AutorisasjonValgt
    | SertifiseringValgt
    | AnnenErfaringValgt
    | KursValgt
    | FørerkortValgt


type InputId
    = KlarTilÅBegynneId
    | LeggTilAutorisasjonerId
    | LeggTilAnnetId
    | BekreftSammendragId
    | SammendragId
    | DelMedArbeidsgiverId
    | VidereforTilEuresId
    | GiTilbakemeldingId
    | TilbakemeldingLenkeId
    | LagringFeiletActionId
    | AvsluttId


inputIdTilString : InputId -> String
inputIdTilString inputId =
    case inputId of
        KlarTilÅBegynneId ->
            "klar-til-å-begynne-id"

        LeggTilAutorisasjonerId ->
            "legg-til-autorisasjoner-id"

        LeggTilAnnetId ->
            "legg-til-annet-id"

        BekreftSammendragId ->
            "bekreft-sammendrag-id"

        SammendragId ->
            "sammendrag-input"

        LagringFeiletActionId ->
            "sammendrag-lagring-feilet-id"

        DelMedArbeidsgiverId ->
            "del-med-arbeidsgiver-id"

        VidereforTilEuresId ->
            "viderefor-til-eures-id"

        GiTilbakemeldingId ->
            "gi-tilbakemelding-id"

        TilbakemeldingLenkeId ->
            "tilbakemelding-lenke-id"

        AvsluttId ->
            "avslutt-id"


settFokus : Samtale -> Cmd SuccessMsg
settFokus samtale =
    case samtale of
        Introduksjon _ ->
            settFokusCmd KlarTilÅBegynneId

        LeggTilAutorisasjoner ->
            settFokusCmd LeggTilAutorisasjonerId

        LeggTilFlereAutorisasjoner ->
            settFokusCmd LeggTilAutorisasjonerId

        LeggTilAnnet ->
            settFokusCmd LeggTilAnnetId

        LeggTilFlereAnnet ->
            settFokusCmd LeggTilAnnetId

        BekreftSammendrag _ _ ->
            settFokusCmd BekreftSammendragId

        EndrerSammendrag _ _ ->
            settFokusCmd SammendragId

        SkriverSammendrag _ _ ->
            settFokusCmd SammendragId

        LagringAvSammendragFeilet _ _ ->
            settFokusCmd LagringFeiletActionId

        DelMedArbeidsgiver ->
            settFokusCmd DelMedArbeidsgiverId

        InformerOmEures _ ->
            settFokusCmd VidereforTilEuresId

        SpørOmTilbakemelding ->
            settFokusCmd GiTilbakemeldingId

        Avslutt _ ->
            settFokusCmd AvsluttId

        _ ->
            Cmd.none


settFokusCmd : InputId -> Cmd SuccessMsg
settFokusCmd inputId =
    Process.sleep 200
        |> Task.andThen (\_ -> (inputIdTilString >> Dom.focus) inputId)
        |> Task.attempt FokusSatt


updateAndreSamtaleSteg : SuccessModel -> AndreSamtaleStegMsg -> AndreSamtaleStegInfo -> ( SuccessModel, Cmd SuccessMsg )
updateAndreSamtaleSteg model msg info =
    case msg of
        BrukerSierHeiIIntroduksjonen ->
            case info.aktivSamtale of
                Introduksjon _ ->
                    let
                        ( personaliaModel, personaliaCmd ) =
                            info.meldingsLogg
                                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja!" ])
                                |> Personalia.Seksjon.init model.debugStatus (Cv.sistEndretDato model.cv) model.personalia
                    in
                    ( personaliaModel
                        |> PersonaliaSeksjon
                        |> oppdaterSamtaleSeksjon model
                    , Cmd.map PersonaliaMsg personaliaCmd
                    )

                _ ->
                    ( model, Cmd.none )

        SeksjonValgt valgtSeksjon ->
            gåTilValgtSeksjon model info msg valgtSeksjon

        IngenAvAutorisasjonSeksjoneneValgt ->
            ( LeggTilAnnet
                |> oppdaterSamtale model info (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )

        IngenAvDeAndreSeksjoneneValgt ->
            gåVidereFraSeksjonsvalg model info

        VilSeEksempel ->
            case info.aktivSamtale of
                SkriverSammendrag _ tekst ->
                    tekst
                        |> SkriverSammendrag False
                        |> visEksemplerSammendrag model info msg

                EndrerSammendrag _ tekst ->
                    tekst
                        |> EndrerSammendrag False
                        |> visEksemplerSammendrag model info msg

                _ ->
                    ( model, Cmd.none )

        BrukerVilEndreSammendrag ->
            case info.aktivSamtale of
                BekreftSammendrag medEksempelKnapp bekreftSammendragState ->
                    case bekreftSammendragState of
                        OpprinneligSammendrag sammendrag ->
                            sammendrag
                                |> Sammendrag.toString
                                |> gåTilEndreSammendrag model info msg medEksempelKnapp

                        NyttSammendrag sammendrag ->
                            gåTilEndreSammendrag model info msg medEksempelKnapp sammendrag

                        EndretSammendrag sammendrag ->
                            gåTilEndreSammendrag model info msg medEksempelKnapp sammendrag

                _ ->
                    ( model, Cmd.none )

        SammendragEndret tekst ->
            case info.aktivSamtale of
                EndrerSammendrag medEksempelKnapp _ ->
                    ( tekst
                        |> EndrerSammendrag medEksempelKnapp
                        |> oppdaterSamtale model info IngenNyeMeldinger
                    , Cmd.none
                    )

                SkriverSammendrag medEksempelKnapp _ ->
                    ( tekst
                        |> SkriverSammendrag medEksempelKnapp
                        |> oppdaterSamtale model info IngenNyeMeldinger
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        VilLagreSammendragSkjema ->
            case info.aktivSamtale of
                EndrerSammendrag medEksempelKnapp tekst ->
                    case Validering.feilmeldingMaxAntallTegn tekst 4000 of
                        Just _ ->
                            ( model, Cmd.none )

                        Nothing ->
                            ( tekst
                                |> EndretSammendrag
                                |> BekreftSammendrag medEksempelKnapp
                                |> oppdaterSamtale model info (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                SkriverSammendrag medEksempelKnapp tekst ->
                    case Validering.feilmeldingMaxAntallTegn tekst 4000 of
                        Just _ ->
                            ( model, Cmd.none )

                        Nothing ->
                            ( tekst
                                |> NyttSammendrag
                                |> BekreftSammendrag medEksempelKnapp
                                |> oppdaterSamtale model info (SvarFraMsg msg)
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                _ ->
                    ( model, Cmd.none )

        VilLagreBekreftetSammendrag ->
            case info.aktivSamtale of
                LagringAvSammendragFeilet error feiletSammendrag ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSammendrag feiletSammendrag
                        |> oppdaterSamtale model info (SvarFraMsg msg)
                    , Api.endreSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) feiletSammendrag
                    )

                BekreftSammendrag _ bekreftSammendragState ->
                    case bekreftSammendragState of
                        OpprinneligSammendrag _ ->
                            { info
                                | meldingsLogg =
                                    info.meldingsLogg
                                        |> MeldingsLogg.leggTilSvar (svarFraBrukerInput info msg)
                            }
                                |> gåTilJobbprofilSjekk model

                        NyttSammendrag sammendrag ->
                            ( LagreStatus.init
                                |> LagrerSammendrag sammendrag
                                |> oppdaterSamtale model info (SvarFraMsg msg)
                            , Api.endreSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) sammendrag
                            )

                        EndretSammendrag sammendrag ->
                            ( LagreStatus.init
                                |> LagrerSammendrag sammendrag
                                |> oppdaterSamtale model info (SvarFraMsg msg)
                            , Api.endreSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) sammendrag
                            )

                _ ->
                    ( model, Cmd.none )

        VilIkkeLagreSammendrag ->
            { info
                | meldingsLogg =
                    info.meldingsLogg
                        |> MeldingsLogg.leggTilSvar (svarFraBrukerInput info msg)
            }
                |> gåTilJobbprofilSjekk model

        SammendragOppdatert result ->
            case info.aktivSamtale of
                LagrerSammendrag sammendrag lagreStatus ->
                    case result of
                        Ok sammendragInfo ->
                            gåTilJobbprofilSjekk model
                                { info
                                    | sistLagret = Sammendrag.sistEndretDato sammendragInfo
                                    , meldingsLogg =
                                        if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                            info.meldingsLogg
                                                |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])
                                                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Veldig bra! Nå er vi ferdig med det vanskeligste 😊" ] ]

                                        else
                                            info.meldingsLogg
                                                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Veldig bra! Nå er vi ferdig med det vanskeligste 😊" ] ]
                                }

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( LagreStatus.fraError error
                                        |> LagrerSammendrag sammendrag
                                        |> oppdaterSamtale model info IngenNyeMeldinger
                                    , Api.endreSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) sammendrag
                                    )

                                else
                                    ( sammendrag
                                        |> LagringAvSammendragFeilet error
                                        |> oppdaterSamtale model info IngenNyeMeldinger
                                    , sammendrag
                                        |> Sammendrag.encodeSammendrag
                                        |> Api.logErrorWithRequestBody (AndreSamtaleStegMsg ErrorLogget) "Lagre sammendrag" error
                                    )

                            else
                                ( sammendrag
                                    |> LagringAvSammendragFeilet error
                                    |> oppdaterSamtale model info UtenSvar
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , sammendrag
                                        |> Sammendrag.encodeSammendrag
                                        |> Api.logErrorWithRequestBody (AndreSamtaleStegMsg ErrorLogget) "Lagre sammendrag" error
                                    ]
                                )

                _ ->
                    ( model, Cmd.none )

        BrukerGodkjennerSynligCV ->
            ( LagreStatus.init
                |> LagrerSynlighet True
                |> oppdaterSamtale model info (SvarFraMsg msg)
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.endreSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) True
                ]
            )

        BrukerGodkjennerIkkeSynligCV ->
            ( LagreStatus.init
                |> LagrerSynlighet False
                |> oppdaterSamtale model info (SvarFraMsg msg)
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.endreSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) False
                ]
            )

        HarSvartJaTilEures ->
            ( SpørOmTilbakemelding
                |> oppdaterSamtale model info (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )

        HarSvartNeiTilEures ->
            ( SpørOmTilbakemelding
                |> oppdaterSamtale model info (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )

        VilGiTilbakemelding ->
            ( Avslutt True
                |> oppdaterSamtale model info (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )

        VilIkkeGiTilbakemelding ->
            ( Avslutt False
                |> oppdaterSamtale model info (SvarFraMsg msg)
            , lagtTilSpørsmålCmd model.debugStatus
            )

        SynlighetPostet result ->
            case info.aktivSamtale of
                LagrerSynlighet skalVæreSynlig lagreStatus ->
                    case result of
                        Ok _ ->
                            if skalVæreSynlig then
                                let
                                    oppdatertMeldingsLogg =
                                        if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                            info.meldingsLogg
                                                |> MeldingsLogg.leggTilSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])

                                        else
                                            info.meldingsLogg
                                in
                                gåTilJobbprofil (Cv.sistEndretDato model.cv) (JobbSkifter Synlig) model { info | meldingsLogg = oppdatertMeldingsLogg }

                            else
                                -- Kun jobbskiftere får valget om å velge synlighet, hvis de svarer nei, sender vi de til tilbakemelding
                                ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                    InformerOmEures False
                                        |> oppdaterSamtale model info (ManueltSvar (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ]))

                                  else
                                    InformerOmEures False
                                        |> oppdaterSamtale model info UtenSvar
                                , lagtTilSpørsmålCmd model.debugStatus
                                )

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSynlighet skalVæreSynlig
                                        |> oppdaterSamtale model info IngenNyeMeldinger
                                    , Api.endreSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) skalVæreSynlig
                                    )

                                else
                                    ( skalVæreSynlig
                                        |> LagringSynlighetFeilet error
                                        |> oppdaterSamtale model info IngenNyeMeldinger
                                    , error
                                        |> Feilmelding.feilmelding "Lagre synlighet"
                                        |> Maybe.map (Api.logError (always ErrorLogget >> AndreSamtaleStegMsg))
                                        |> Maybe.withDefault Cmd.none
                                    )

                            else
                                ( skalVæreSynlig
                                    |> LagringSynlighetFeilet error
                                    |> oppdaterSamtale model info UtenSvar
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , error
                                        |> Feilmelding.feilmelding "Lagre synlighet"
                                        |> Maybe.map (Api.logError (always ErrorLogget >> AndreSamtaleStegMsg))
                                        |> Maybe.withDefault Cmd.none
                                    ]
                                )

                _ ->
                    ( model, Cmd.none )

        BrukerVilPrøveÅLagreSynlighetPåNytt ->
            case info.aktivSamtale of
                LagringSynlighetFeilet error skalVæreSynlig ->
                    ( error
                        |> LagreStatus.fraError
                        |> LagrerSynlighet skalVæreSynlig
                        |> oppdaterSamtale model info IngenNyeMeldinger
                    , Api.endreSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) skalVæreSynlig
                    )

                _ ->
                    ( model, Cmd.none )

        BrukerGirOppÅLagreSynlighet skalVæreSynlig ->
            if skalVæreSynlig then
                let
                    oppdatertMeldingslogg =
                        info.meldingsLogg
                            |> MeldingsLogg.leggTilSvar (svarFraBrukerInput info msg)
                            |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Ok. Du kan gjøre CV-en søkbar senere på Min side." ] ]
                in
                gåTilJobbprofil (Cv.sistEndretDato model.cv) (JobbSkifter IkkeSynlig) model { info | meldingsLogg = oppdatertMeldingslogg }

            else
                ( InformerOmEures False
                    |> oppdaterSamtale model info (SvarFraMsg msg)
                , lagtTilSpørsmålCmd model.debugStatus
                )

        WindowEndrerVisibility visibility ->
            case visibility of
                Visible ->
                    case info.aktivSamtale of
                        LagringAvSammendragFeilet error feiletSammendrag ->
                            case ErrorHåndtering.operasjonEtterError error of
                                LoggInn ->
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSammendrag feiletSammendrag
                                        |> oppdaterSamtale model info IngenNyeMeldinger
                                    , Api.endreSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) feiletSammendrag
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        LagrerSammendrag sammendrag lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSammendrag sammendrag
                                |> oppdaterSamtale model info IngenNyeMeldinger
                            , Cmd.none
                            )

                        LagringSynlighetFeilet error skalVæreSynlig ->
                            case ErrorHåndtering.operasjonEtterError error of
                                LoggInn ->
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSynlighet skalVæreSynlig
                                        |> oppdaterSamtale model info IngenNyeMeldinger
                                    , Api.endreSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) skalVæreSynlig
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        LagrerSynlighet skalVæreSynlig lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSynlighet skalVæreSynlig
                                |> oppdaterSamtale model info IngenNyeMeldinger
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Hidden ->
                    ( model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            let
                ( nyMeldingslogg, cmd ) =
                    SamtaleAnimasjon.update model.debugStatus samtaleAnimasjonMsg info.meldingsLogg
            in
            case MeldingsLogg.ferdigAnimert nyMeldingslogg of
                FerdigAnimert ferdigAnimertSamtale ->
                    case info.aktivSamtale of
                        VenterPåÅGåTilJobbprofil brukerInfo ->
                            gåTilJobbprofil (Cv.sistEndretDato model.cv) brukerInfo model { info | meldingsLogg = nyMeldingslogg }

                        _ ->
                            ( { info | meldingsLogg = nyMeldingslogg }
                                |> AndreSamtaleSteg
                                |> oppdaterSamtaleSeksjon model
                            , Cmd.batch
                                [ cmd
                                    |> Cmd.map (SamtaleAnimasjonMsg >> AndreSamtaleStegMsg)
                                , settFokus info.aktivSamtale
                                ]
                            )

                MeldingerGjenstår ->
                    ( { info | meldingsLogg = nyMeldingslogg }
                        |> AndreSamtaleSteg
                        |> oppdaterSamtaleSeksjon model
                    , cmd
                        |> Cmd.map (SamtaleAnimasjonMsg >> AndreSamtaleStegMsg)
                    )

        ErrorLogget ->
            ( model, Cmd.none )


gåTilValgtSeksjon : SuccessModel -> AndreSamtaleStegInfo -> AndreSamtaleStegMsg -> ValgtSeksjon -> ( SuccessModel, Cmd SuccessMsg )
gåTilValgtSeksjon model info msg valgtSeksjon =
    let
        sistLagret =
            info.sistLagret

        meldingsLogg =
            info.meldingsLogg
                |> MeldingsLogg.leggTilSvar (svarFraBrukerInput info msg)
    in
    case MeldingsLogg.ferdigAnimert meldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            case valgtSeksjon of
                FagbrevSvennebrevValgt ->
                    gåTilFagbrev sistLagret model ferdigAnimertMeldingsLogg

                MesterbrevValgt ->
                    gåTilMesterbrev sistLagret model ferdigAnimertMeldingsLogg

                {- AutorisasjonValgt ->
                   gåTilAutorisasjon sistLagret model ferdigAnimertMeldingsLogg
                -}
                SertifiseringValgt ->
                    gåTilSertifisering sistLagret model ferdigAnimertMeldingsLogg

                AnnenErfaringValgt ->
                    gåTilAnnenErfaring sistLagret model ferdigAnimertMeldingsLogg

                KursValgt ->
                    gåTilKurs sistLagret model ferdigAnimertMeldingsLogg

                FørerkortValgt ->
                    gåTilFørerkort sistLagret model ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( { info | meldingsLogg = meldingsLogg }
                |> AndreSamtaleSteg
                |> oppdaterSamtaleSeksjon model
            , lagtTilSpørsmålCmd model.debugStatus
            )


gåVidereFraSeksjonsvalg : SuccessModel -> AndreSamtaleStegInfo -> ( SuccessModel, Cmd SuccessMsg )
gåVidereFraSeksjonsvalg model info =
    let
        samtale =
            case Cv.sammendrag model.cv of
                Just sammendrag ->
                    if String.isEmpty (String.trim (Sammendrag.toString sammendrag)) then
                        SkriverSammendrag True ""

                    else
                        BekreftSammendrag True (OpprinneligSammendrag sammendrag)

                Nothing ->
                    EndrerSammendrag True ""
    in
    ( { meldingsLogg =
            info.meldingsLogg
                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Nei, gå videre" ])
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
      , aktivSamtale = samtale
      , sistLagret = info.sistLagret
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )


gåTilEndreSammendrag : SuccessModel -> AndreSamtaleStegInfo -> AndreSamtaleStegMsg -> Bool -> String -> ( SuccessModel, Cmd SuccessMsg )
gåTilEndreSammendrag model info msg medEksempelKnapp sammendragTekst =
    ( sammendragTekst
        |> EndrerSammendrag medEksempelKnapp
        |> oppdaterSamtale model info (SvarFraMsg msg)
    , lagtTilSpørsmålCmd model.debugStatus
    )


visEksemplerSammendrag : SuccessModel -> AndreSamtaleStegInfo -> AndreSamtaleStegMsg -> Samtale -> ( SuccessModel, Cmd SuccessMsg )
visEksemplerSammendrag model info msg aktivSamtale =
    let
        oppdatertMeldingslogg =
            info.meldingsLogg
                |> MeldingsLogg.leggTilSvar (svarFraBrukerInput info msg)
                |> MeldingsLogg.leggTilSpørsmål eksemplerPåSammendrag
    in
    ( aktivSamtale
        |> oppdaterSamtale model { info | meldingsLogg = oppdatertMeldingslogg } IngenNyeMeldinger
    , lagtTilSpørsmålCmd model.debugStatus
    )


gåTilJobbprofilSjekk : SuccessModel -> AndreSamtaleStegInfo -> ( SuccessModel, Cmd SuccessMsg )
gåTilJobbprofilSjekk model info =
    let
        brukerInfo =
            Person.brukerInfo model.person
    in
    case brukerInfo of
        UnderOppfølging _ ->
            gåTilJobbprofil (Cv.sistEndretDato model.cv) brukerInfo model info

        JobbSkifter _ ->
            ( DelMedArbeidsgiver
                |> oppdaterSamtale model info UtenSvar
            , lagtTilSpørsmålCmd model.debugStatus
            )


gåTilJobbprofil : Posix -> BrukerInfo -> SuccessModel -> AndreSamtaleStegInfo -> ( SuccessModel, Cmd SuccessMsg )
gåTilJobbprofil sistLagret brukerInfo model info =
    case MeldingsLogg.ferdigAnimert info.meldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            let
                ( jobbprofilModel, jobbprofilCmd ) =
                    Jobbprofil.Seksjon.init model.debugStatus sistLagret brukerInfo ferdigAnimertMeldingsLogg
            in
            ( { model
                | aktivSeksjon = JobbprofilSeksjon jobbprofilModel
              }
            , Cmd.map JobbprofilMsg jobbprofilCmd
            )

        MeldingerGjenstår ->
            ( VenterPåÅGåTilJobbprofil brukerInfo
                |> oppdaterSamtale model info IngenNyeMeldinger
            , lagtTilSpørsmålCmd model.debugStatus
            )


svarFraBrukerInput : AndreSamtaleStegInfo -> AndreSamtaleStegMsg -> Melding
svarFraBrukerInput modelInfo msg =
    modelInfo
        |> andreSamtaleStegTilBrukerInput
        |> BrukerInput.tilSvarMelding msg


oppdaterSamtale : SuccessModel -> AndreSamtaleStegInfo -> SamtaleOppdatering AndreSamtaleStegMsg -> Samtale -> SuccessModel
oppdaterSamtale model info meldingsoppdatering samtale =
    { info
        | aktivSamtale = samtale
        , meldingsLogg =
            case meldingsoppdatering of
                IngenNyeMeldinger ->
                    info.meldingsLogg

                SvarFraMsg msg ->
                    info.meldingsLogg
                        |> MeldingsLogg.leggTilSvar (svarFraBrukerInput info msg)
                        |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                ManueltSvar melding ->
                    info.meldingsLogg
                        |> MeldingsLogg.leggTilSvar melding
                        |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)

                UtenSvar ->
                    info.meldingsLogg
                        |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
    }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg samtale =
    case samtale of
        Introduksjon personalia ->
            [ Melding.spørsmål [ "Hei, " ++ (Personalia.fornavn personalia |> Maybe.withDefault "") ++ ", nå starter vi på CV-en din!" ]
            , Melding.spørsmål [ "Først legger du inn utdanning, arbeidserfaring, språk og førerkort. Etter det kan du legge inn fagbrev, kurs, sertifisering og sammendrag." ]
            , Melding.spørsmål [ "Du skal ikke skrive inn noe om helse, religion eller politiske oppfatning." ]
            , Melding.spørsmål [ "Er du klar til å begynne?" ]
            ]

        LeggTilAutorisasjoner ->
            [ Melding.spørsmål [ "Nå har du lagt inn mye i CV-en din. Er det noe mer du kan ta med?" ]
            , Melding.spørsmål [ "Vil du legge til noen av disse kategoriene?" ]
            ]

        LeggTilFlereAutorisasjoner ->
            []

        LeggTilAnnet ->
            [ Melding.spørsmål [ "Det er viktig å få med alt du kan på CV-en." ]
            , Melding.spørsmål [ "Har du jobbet som frivillig eller har hatt verv? Legg til annen erfaring." ]
            , Melding.spørsmål [ "Har du tatt norskprøve? Legg til kurs." ]
            , Melding.spørsmål [ "Vil du legge til noen av disse kategoriene?" ]
            ]

        LeggTilFlereAnnet ->
            []

        BekreftSammendrag _ bekreftState ->
            case bekreftState of
                OpprinneligSammendrag sammendrag ->
                    [ Melding.spørsmål [ "Supert, nå er vi snart ferdig med CV-en." ]
                    , Melding.spørsmål [ "Nå skal du skrive et sammendrag. Her har du mulighet til å selge deg inn. Fortell arbeidsgivere om kompetansen din og personlige egenskaper." ]
                    , Melding.spørsmål
                        [ "Du har allerede skrevet dette:"
                        , Melding.tomLinje
                        , Sammendrag.toString sammendrag
                        , Melding.tomLinje
                        , "Er du fornøyd? "
                        ]
                    ]

                NyttSammendrag sammendrag ->
                    [ Melding.spørsmål
                        [ "Du la inn dette:"
                        , Melding.tomLinje
                        , sammendrag
                        , Melding.tomLinje
                        , "Er du fornøyd? "
                        ]
                    ]

                EndretSammendrag _ ->
                    [ Melding.spørsmål [ "Nå har du endret. Er du fornøyd?" ] ]

        EndrerSammendrag _ _ ->
            [ Melding.spørsmål [ "Gjør endringene du ønsker." ] ]

        SkriverSammendrag _ _ ->
            [ Melding.spørsmål [ "Supert, nå er vi snart ferdig med CV-en." ]
            , Melding.spørsmål [ "Nå skal du skrive et sammendrag. Her har du mulighet til å selge deg inn. Fortell arbeidsgivere om kompetansen din og personlige egenskaper." ]
            , Melding.spørsmål [ "Skriv sammendraget ditt i boksen under." ]
            ]

        LagrerSammendrag _ _ ->
            []

        LagringAvSammendragFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre sammendrag" } ]

        VenterPåÅGåTilJobbprofil _ ->
            []

        DelMedArbeidsgiver ->
            [ Melding.spørsmål [ "Noen arbeidsgivere søker aktivt i CV-ene på Arbeidsplassen. Da kan de kontakte deg direkte." ]
            , Melding.spørsmål [ "Vil du la arbeidsgivere søke opp CV-en din?" ]
            ]

        InformerOmEures harLagtInnJobbprofil ->
            if harLagtInnJobbprofil then
                [ Melding.spørsmål [ "Kunne du tenkt deg å jobbe utenfor Norge? Legger du også inn din CV på EURES-portalen kan du bli funnet av arbeidsgivere fra flere Europeiske land." ]
                ]

            else
                [ Melding.spørsmål [ "Ok. Du kan gjøre CV-en søkbar senere på Min side." ]
                , Melding.spørsmål [ "Kunne du tenkt deg å jobbe utenfor Norge? Legger du også inn din CV på EURES-portalen kan du bli funnet av arbeidsgivere fra flere Europeiske land." ]
                ]

        SpørOmTilbakemelding ->
            [ Melding.spørsmål [ "Hvis du har tid, vil jeg gjerne vite hvordan du synes det var å lage CV-en. Du kan svare på 3 spørsmål, og du er anonym 😊 Vil du svare (det er frivillig)?" ]
            ]

        Avslutt harGittTilbakemelding ->
            if harGittTilbakemelding then
                [ Melding.spørsmål [ "Takk for tilbakemeldingen. Lykke til med jobbjakten! 😊" ]
                ]

            else
                [ Melding.spørsmål [ "Lykke til med jobbjakten! 😊" ]
                ]

        LagrerSynlighet _ _ ->
            []

        LagringSynlighetFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre synlighet" } ]


lagtTilSpørsmålCmd : DebugStatus -> Cmd SuccessMsg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map (SamtaleAnimasjonMsg >> AndreSamtaleStegMsg)


eksemplerPåSammendrag : List Melding
eksemplerPåSammendrag =
    [ Melding.eksempelMedTittel "Eksempel 1:"
        [ "Jeg er student, for tiden avslutter jeg mastergrad i økonomi og administrasjon ved Universitetet i Stavanger. Masteroppgaven min handler om endringsledelse i oljenæringen. Ved siden av studiene har jeg jobbet som guide på Norsk Oljemuseum."
        , Melding.tomLinje
        , "På fritiden spiller jeg fotball og sitter i styret til studentidrettslaget."
        , Melding.tomLinje
        , "Jeg er strukturert og løsningsorientert. Mine tidligere arbeidsgivere har beskrevet meg som effektiv, ansvarsbevisst og positiv."
        ]
    , Melding.eksempelMedTittel "Eksempel 2:"
        [ "- Fagbrev i logistikk og transport"
        , "- 16 års erfaring fra lager og logistikk"
        , "- Har hatt hovedansvar for varemottak og forsendelser"
        , "- Erfaring med flere logistikksystemer"
        , "- God IT-kompetanse"
        , "- Nøyaktig, fleksibel og har høy arbeidskapasitet"
        ]
    ]


andreSamtaleStegTilMetrikkSeksjon : AndreSamtaleStegInfo -> Metrikker.Seksjon
andreSamtaleStegTilMetrikkSeksjon { aktivSamtale } =
    case aktivSamtale of
        Introduksjon _ ->
            Metrikker.Intro

        LeggTilAutorisasjoner ->
            Metrikker.LeggTilFagdokumentasjoner

        LeggTilFlereAutorisasjoner ->
            Metrikker.LeggTilFagdokumentasjoner

        LeggTilAnnet ->
            Metrikker.LeggTilAnnet

        LeggTilFlereAnnet ->
            Metrikker.LeggTilAnnet

        BekreftSammendrag _ _ ->
            Metrikker.Sammendrag

        SkriverSammendrag _ _ ->
            Metrikker.Sammendrag

        EndrerSammendrag _ _ ->
            Metrikker.Sammendrag

        LagrerSammendrag _ _ ->
            Metrikker.Sammendrag

        LagringAvSammendragFeilet _ _ ->
            Metrikker.Sammendrag

        VenterPåÅGåTilJobbprofil _ ->
            Metrikker.Sammendrag

        DelMedArbeidsgiver ->
            Metrikker.Synlighet

        LagrerSynlighet _ _ ->
            Metrikker.Synlighet

        LagringSynlighetFeilet _ _ ->
            Metrikker.Synlighet

        InformerOmEures _ ->
            Metrikker.Eures

        SpørOmTilbakemelding ->
            Metrikker.Tilbakemelding

        Avslutt _ ->
            Metrikker.Slutten



--- VIEW ---


getMinutesAgo : Int -> String
getMinutesAgo difference =
    ((toFloat difference / (60 * 1000))
        |> floor
        |> String.fromInt
    )
        ++ " minutter siden"


sistLagretToString : ExtendedModel -> Posix -> Maybe String
sistLagretToString extendedModel sistLagret =
    let
        difference =
            abs (Time.posixToMillis extendedModel.time - Time.posixToMillis sistLagret)
    in
    if Time.posixToMillis extendedModel.time == 0 then
        Nothing

    else if difference < (60 * 1000) then
        Just "Nå"

    else if difference < (2 * 60 * 1000) then
        Just "1 minutt siden"

    else if difference < (6 * 60 * 1000) then
        Just (getMinutesAgo difference)

    else if ulikDato extendedModel.timeZone sistLagret extendedModel.time then
        Nothing

    else
        Just (tilKlokkeSlett extendedModel.timeZone sistLagret)


getSistLagret : ExtendedModel -> Maybe String
getSistLagret extendedModel =
    case extendedModel.model of
        Success successModel ->
            case successModel.aktivSeksjon of
                AndreSamtaleSteg info ->
                    info.sistLagret
                        |> sistLagretToString extendedModel

                PersonaliaSeksjon model ->
                    model
                        |> Personalia.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                UtdanningSeksjon model ->
                    model
                        |> Utdanning.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                ArbeidsErfaringSeksjon model ->
                    model
                        |> Arbeidserfaring.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                SpråkSeksjon model ->
                    model
                        |> Sprak.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                FagdokumentasjonSeksjon model ->
                    model
                        |> Fagdokumentasjon.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                SertifikatSeksjon model ->
                    model
                        |> Sertifikat.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                AnnenErfaringSeksjon model ->
                    model
                        |> AnnenErfaring.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                FørerkortSeksjon model ->
                    model
                        |> Forerkort.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                KursSeksjon model ->
                    model
                        |> Kurs.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

                JobbprofilSeksjon model ->
                    model
                        |> Jobbprofil.Seksjon.sistLagret
                        |> sistLagretToString extendedModel

        _ ->
            Nothing


viewDocument : ExtendedModel -> Browser.Document Msg
viewDocument extendedModel =
    { title = "CV-samtale - Arbeidsplassen"
    , body = [ view extendedModel ]
    }


view : ExtendedModel -> Html Msg
view extendedModel =
    div [ class "app" ]
        [ case extendedModel.modalStatus of
            TilbakemeldingModalÅpen modalModel ->
                modalModel
                    |> TilbakemeldingModal.view
                    |> Html.map ModalMsg

            ModalLukket ->
                text ""
        , div [ ariaHidden (extendedModel.modalStatus /= ModalLukket) ]
            [ { windowWidth = extendedModel.windowWidth
              , onAvsluttClick = ÅpneTilbakemeldingModal
              , aktivSeksjon = modelTilMetrikkSeksjon extendedModel.model
              , sistLagret = getSistLagret extendedModel
              }
                |> Header.header
                |> Header.toHtml
            , case extendedModel.model of
                Loading _ ->
                    viewLoading

                Success successModel ->
                    viewSuccess successModel

                Failure error ->
                    div [ class "failure-wrapper" ]
                        [ div [ class "failure" ]
                            [ Alertstripe.alertstripeFeil
                                [ text (ErrorHåndtering.feilmeldingEtterErrorILoading error)
                                ]
                                |> Alertstripe.toHtml
                            ]
                        ]
            ]
        ]


viewLoading : Html msg
viewLoading =
    div [ class "spinner-wrapper" ]
        [ Spinner.spinner
            |> Spinner.withStørrelse Spinner.L
            |> Spinner.toHtml
        ]


meldingsLoggFraSeksjon : SamtaleSeksjon -> MeldingsLogg
meldingsLoggFraSeksjon aktivSeksjon =
    case aktivSeksjon of
        PersonaliaSeksjon model ->
            Personalia.Seksjon.meldingsLogg model

        UtdanningSeksjon model ->
            Utdanning.Seksjon.meldingsLogg model

        ArbeidsErfaringSeksjon model ->
            Arbeidserfaring.Seksjon.meldingsLogg model

        SpråkSeksjon model ->
            Sprak.Seksjon.meldingsLogg model

        FagdokumentasjonSeksjon model ->
            Fagdokumentasjon.Seksjon.meldingsLogg model

        SertifikatSeksjon model ->
            Sertifikat.Seksjon.meldingsLogg model

        AnnenErfaringSeksjon model ->
            AnnenErfaring.Seksjon.meldingsLogg model

        FørerkortSeksjon model ->
            Forerkort.Seksjon.meldingsLogg model

        KursSeksjon model ->
            Kurs.Seksjon.meldingsLogg model

        AndreSamtaleSteg andreSamtaleStegInfo ->
            andreSamtaleStegInfo.meldingsLogg

        JobbprofilSeksjon model ->
            Jobbprofil.Seksjon.meldingsLogg model


viewSuccess : SuccessModel -> Html Msg
viewSuccess successModel =
    div [ class "cv-samtale", id "samtale" ]
        [ div [ id "samtale-innhold" ]
            [ div [ class "samtale-header" ]
                [ i [ class "Robotlogo-header" ] []
                , h1 [] [ text "Få hjelp til å lage CV-en" ]
                , p [] [ text "Her starter samtalen din med roboten" ]
                ]
            , div [ class "samtale-wrapper" ]
                [ div [ class "samtale" ]
                    [ successModel.aktivSeksjon
                        |> meldingsLoggFraSeksjon
                        |> viewMeldingsLogg
                    , viewBrukerInput successModel.aktivSeksjon
                    , div [ class "samtale-padding" ] []
                    ]
                ]
            ]
        ]


viewMeldingsLogg : MeldingsLogg -> Html msg
viewMeldingsLogg meldingsLogg =
    meldingsLogg
        |> MeldingsLogg.mapMeldingsGruppe viewMeldingsgruppe
        |> div []


viewMeldingsgruppe : MeldingsGruppeViewState -> Html msg
viewMeldingsgruppe meldingsGruppe =
    case meldingsGruppe of
        SpørsmålGruppe spørsmålGruppe ->
            spørsmålGruppe
                |> MeldingsLogg.mapSpørsmålsgruppe viewSpørsmål
                |> div [ class "meldingsgruppe", ariaLabel "Roboten" ]

        SvarGruppe melding ->
            viewSvar melding


viewSpørsmål : SpørsmålViewState -> Html msg
viewSpørsmål spørsmål =
    let
        spørsmålClass =
            case SpørsmålViewState.meldingsType spørsmål of
                Spørsmål ->
                    "melding "

                SpørsmålMedEksempel ->
                    "melding eksempel "

                Svar ->
                    ""
    in
    div [ class "meldingsrad sporsmal" ]
        [ div [ class "robot", robotAttribute spørsmål ]
            [ i [ class "Robotlogo" ] [] ]
        , case SpørsmålViewState.spørsmålStyle spørsmål of
            FørSkriveindikator ->
                div
                    [ class (spørsmålClass ++ "skjult")
                    , ariaLive "off"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ div [ class "skriver-melding" ] [] ]

            Skriveindikator ->
                div
                    [ class (spørsmålClass ++ "skriveindikator")
                    , ariaLive "off"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ viewSkriveStatus ]

            StørrelseKalkuleres ->
                article
                    [ class (spørsmålClass ++ "kalkulerer")
                    , ariaLive "polite"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ div [ class "meldinginnhold-overflow-hidden" ]
                        [ div [ class "meldinginnhold-wrapper", id "test" ]
                            (spørsmål
                                |> SpørsmålViewState.tekst
                                |> List.map viewTekstområde
                            )
                        ]
                    ]

            MeldingAnimeres { height, width } ->
                let
                    padding =
                        16

                    snakkebobleHeight =
                        Konstanter.meldingHøyde height

                    snakkebobleWidth =
                        width + (2 * padding) + 1
                in
                article
                    [ class (spørsmålClass ++ "ferdiganimert")
                    , ariaLive "polite"
                    , style "height" (String.fromInt snakkebobleHeight ++ "px")
                    , style "width" (String.fromInt snakkebobleWidth ++ "px")
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ div [ class "meldinginnhold-overflow-hidden" ]
                        [ div [ class "meldinginnhold-wrapper" ]
                            (spørsmål
                                |> SpørsmålViewState.tekst
                                |> List.map viewTekstområde
                            )
                        ]
                    ]

            MeldingFerdigAnimert ->
                article
                    [ class spørsmålClass
                    , classList [ ( "ikke-siste", ikkeSisteMelding spørsmål ) ]
                    , ariaLive "polite"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    (spørsmål
                        |> SpørsmålViewState.tekst
                        |> List.map viewTekstområde
                    )
        ]


ikkeSisteMelding : SpørsmålViewState -> Bool
ikkeSisteMelding spørsmål =
    case SpørsmålViewState.ikonStatus spørsmål of
        SkjultIkon ->
            True

        MidtstiltIkonForFørsteSpørsmål ->
            True

        MidtstiltIkon ->
            False

        IkonForNesteMelding _ ->
            True


robotAttribute : SpørsmålViewState -> Html.Attribute msg
robotAttribute spørsmål =
    case SpørsmålViewState.ikonStatus spørsmål of
        SkjultIkon ->
            class "skjult-robot-ikon"

        MidtstiltIkonForFørsteSpørsmål ->
            class "forste-melding"

        MidtstiltIkon ->
            classList []

        IkonForNesteMelding height ->
            transformForRobot height


transformForRobot : { height : Int } -> Html.Attribute msg
transformForRobot { height } =
    let
        avstand =
            (toFloat (Konstanter.meldingHøyde height + Konstanter.skriveIndikatorHøyde) / 2) + toFloat Konstanter.meldingMarginTop
    in
    style "transform" ("translateY(" ++ String.fromFloat avstand ++ "px)")


viewSvar : Melding -> Html msg
viewSvar melding =
    div [ class "meldingsgruppe", ariaLabel "Deg" ]
        [ div [ class "meldingsrad svar" ]
            [ article
                [ class "melding"
                ]
                (melding
                    |> Melding.innhold
                    |> List.map viewTekstområde
                )
            ]
        ]


viewTekstområde : Tekstområde -> Html msg
viewTekstområde tekstområde =
    case tekstområde of
        Avsnitt tekst ->
            viewAvsnitt tekst

        Seksjon labelTekst tekster ->
            section [ ariaLabel labelTekst ]
                (List.map viewAvsnitt tekster)

        Overskrift tekst ->
            span [ class "eksempel-tittel" ] [ text tekst ]


viewAvsnitt : String -> Html msg
viewAvsnitt string =
    p [] [ text string ]


viewSkriveStatus : Html msg
viewSkriveStatus =
    div [ class "skriver-melding" ]
        [ div [ class "bounce bounce1" ] []
        , div [ class "bounce bounce2" ] []
        , div [ class "bounce bounce3" ] []
        ]


viewBrukerInput : SamtaleSeksjon -> Html Msg
viewBrukerInput aktivSeksjon =
    div [ classList [ ( "brukerInput-padding", brukerInputVises aktivSeksjon ) ] ]
        [ viewBrukerInputForSeksjon aktivSeksjon
        ]


brukerInputVises : SamtaleSeksjon -> Bool
brukerInputVises aktivSeksjon =
    aktivSeksjon
        |> meldingsLoggFraSeksjon
        |> MeldingsLogg.visBrukerInput


viewBrukerInputForSeksjon : SamtaleSeksjon -> Html Msg
viewBrukerInputForSeksjon aktivSeksjon =
    case aktivSeksjon of
        PersonaliaSeksjon personaliaSeksjon ->
            personaliaSeksjon
                |> Personalia.Seksjon.viewBrukerInput
                |> Html.map (PersonaliaMsg >> SuccessMsg)

        UtdanningSeksjon utdanningSeksjon ->
            utdanningSeksjon
                |> Utdanning.Seksjon.viewBrukerInput
                |> Html.map (UtdanningsMsg >> SuccessMsg)

        SpråkSeksjon språkSeksjon ->
            språkSeksjon
                |> Sprak.Seksjon.viewBrukerInput
                |> Html.map (SpråkMsg >> SuccessMsg)

        ArbeidsErfaringSeksjon arbeidserfaringSeksjon ->
            arbeidserfaringSeksjon
                |> Arbeidserfaring.Seksjon.viewBrukerInput
                |> Html.map (ArbeidserfaringsMsg >> SuccessMsg)

        FagdokumentasjonSeksjon fagbrevSeksjon ->
            fagbrevSeksjon
                |> Fagdokumentasjon.Seksjon.viewBrukerInput
                |> Html.map (FagdokumentasjonMsg >> SuccessMsg)

        SertifikatSeksjon sertifikatSeksjon ->
            sertifikatSeksjon
                |> Sertifikat.Seksjon.viewBrukerInput
                |> Html.map (SertifikatMsg >> SuccessMsg)

        AnnenErfaringSeksjon annenErfaringSeksjon ->
            annenErfaringSeksjon
                |> AnnenErfaring.Seksjon.viewBrukerInput
                |> Html.map (AnnenErfaringMsg >> SuccessMsg)

        FørerkortSeksjon førerkortSeksjon ->
            førerkortSeksjon
                |> Forerkort.Seksjon.viewBrukerInput
                |> Html.map (FørerkortMsg >> SuccessMsg)

        KursSeksjon kursSeksjon ->
            kursSeksjon
                |> Kurs.Seksjon.viewBrukerInput
                |> Html.map (KursMsg >> SuccessMsg)

        AndreSamtaleSteg andreSamtaleStegInfo ->
            case andreSamtaleStegInfo.aktivSamtale of
                LeggTilAutorisasjoner ->
                    if MeldingsLogg.visBrukerInput andreSamtaleStegInfo.meldingsLogg then
                        div []
                            [ viewBrukerInputForAndreSamtaleSteg andreSamtaleStegInfo
                                |> Html.map (AndreSamtaleStegMsg >> SuccessMsg)
                            ]

                    else
                        text ""

                _ ->
                    viewBrukerInputForAndreSamtaleSteg andreSamtaleStegInfo
                        |> Html.map (AndreSamtaleStegMsg >> SuccessMsg)

        JobbprofilSeksjon jobbprofilSeksjon ->
            jobbprofilSeksjon
                |> Jobbprofil.Seksjon.viewBrukerInput
                |> Html.map (JobbprofilMsg >> SuccessMsg)


viewBrukerInputForAndreSamtaleSteg : AndreSamtaleStegInfo -> Html AndreSamtaleStegMsg
viewBrukerInputForAndreSamtaleSteg info =
    info
        |> andreSamtaleStegTilBrukerInput
        |> BrukerInput.toHtml


andreSamtaleStegTilBrukerInput : AndreSamtaleStegInfo -> BrukerInput AndreSamtaleStegMsg
andreSamtaleStegTilBrukerInput info =
    if MeldingsLogg.visBrukerInput info.meldingsLogg then
        case info.aktivSamtale of
            Introduksjon _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerSierHeiIIntroduksjonen "Ja!"
                        |> Knapp.withId (inputIdTilString KlarTilÅBegynneId)
                    ]

            BekreftSammendrag _ bekreftSammendragState ->
                case bekreftSammendragState of
                    OpprinneligSammendrag _ ->
                        viewBekreftSammendrag

                    NyttSammendrag _ ->
                        viewBekreftSammendrag

                    EndretSammendrag _ ->
                        viewBekreftSammendrag

            EndrerSammendrag medEksempelKnapp sammendrag ->
                viewSammendragInput sammendrag
                    |> BrukerInputMedGåVidereKnapp.textarea VilLagreSammendragSkjema
                    |> BrukerInputMedGåVidereKnapp.withAlternativKnappetekst "Lagre endringer"
                    |> BrukerInputMedGåVidereKnapp.withVisEksempelKnapp medEksempelKnapp VilSeEksempel
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            SkriverSammendrag medEksempelKnapp sammendrag ->
                viewSammendragInput sammendrag
                    |> BrukerInputMedGåVidereKnapp.textarea VilLagreSammendragSkjema
                    |> BrukerInputMedGåVidereKnapp.withVisEksempelKnapp medEksempelKnapp VilSeEksempel
                    |> BrukerInput.brukerInputMedGåVidereKnapp

            LagrerSammendrag _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringAvSammendragFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilIkkeLagreSammendrag "Gå videre uten å lagre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp VilLagreBekreftetSammendrag "Prøv på nytt"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp VilIkkeLagreSammendrag "Gå videre uten å lagre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

            LeggTilAutorisasjoner ->
                viewLeggTilAutorisasjoner

            LeggTilFlereAutorisasjoner ->
                viewLeggTilAutorisasjoner

            LeggTilAnnet ->
                viewLeggTilAnnet

            LeggTilFlereAnnet ->
                viewLeggTilAnnet

            VenterPåÅGåTilJobbprofil _ ->
                BrukerInput.utenInnhold

            DelMedArbeidsgiver ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp BrukerGodkjennerSynligCV "Ja, CV-en min skal være søkbar"
                        |> Knapp.withId (inputIdTilString DelMedArbeidsgiverId)
                    , Knapp.knapp BrukerGodkjennerIkkeSynligCV "Nei, CV-en min skal ikke være søkbar"
                    ]

            InformerOmEures _ ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp HarSvartJaTilEures "Ja, ta meg dit"
                        |> Knapp.withLink "https://ec.europa.eu/eures/public/no/homepage"
                        |> Knapp.withId (inputIdTilString VidereforTilEuresId)
                    , Knapp.knapp HarSvartNeiTilEures "Nei takk"
                    ]

            SpørOmTilbakemelding ->
                BrukerInput.knapper Flytende
                    [ Knapp.knapp VilGiTilbakemelding "Ja, jeg vil svare"
                        |> Knapp.withLink "https://surveys.hotjar.com/s?siteId=118350&surveyId=144585"
                        |> Knapp.withId (inputIdTilString GiTilbakemeldingId)
                    , Knapp.knapp VilIkkeGiTilbakemelding "Nei, jeg vil ikke svare"
                    ]

            Avslutt _ ->
                BrukerInput.lenke
                    (Lenke.lenke
                        { tekst = "Avslutt og vis CV-en min"
                        , url = "/cv-samtale/goto/forhandsvis?utgang=ferdig&seksjon=" ++ Metrikker.seksjonTilString Metrikker.Slutten
                        }
                        |> Lenke.withClass "avslutt-knapp"
                        |> Lenke.withButtonStyle
                        |> Lenke.withId (inputIdTilString AvsluttId)
                    )

            LagrerSynlighet _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    BrukerInput.utenInnhold

            LagringSynlighetFeilet error skalVæreSynlig ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp (BrukerGirOppÅLagreSynlighet skalVæreSynlig) "Gå videre"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            ]

                    PrøvPåNytt ->
                        BrukerInput.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagreSynlighetPåNytt "Prøv på nytt"
                                |> Knapp.withId (inputIdTilString LagringFeiletActionId)
                            , Knapp.knapp (BrukerGirOppÅLagreSynlighet skalVæreSynlig) "Gå videre"
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

    else
        BrukerInput.utenInnhold


viewLeggTilAutorisasjoner : BrukerInput AndreSamtaleStegMsg
viewLeggTilAutorisasjoner =
    BrukerInput.knapper Kolonne
        [ seksjonsvalgKnapp FagbrevSvennebrevValgt
            |> Knapp.withId (inputIdTilString LeggTilAutorisasjonerId)
        , seksjonsvalgKnapp MesterbrevValgt
        , Knapp.knapp IngenAvAutorisasjonSeksjoneneValgt "Nei, gå videre"
        ]


viewLeggTilAnnet : BrukerInput AndreSamtaleStegMsg
viewLeggTilAnnet =
    BrukerInput.knapper Kolonne
        [ seksjonsvalgKnapp AnnenErfaringValgt
            |> Knapp.withId (inputIdTilString LeggTilAnnetId)
        , seksjonsvalgKnapp KursValgt
        , seksjonsvalgKnapp SertifiseringValgt
        , Knapp.knapp IngenAvDeAndreSeksjoneneValgt "Nei, gå videre"
        ]


viewSammendragInput : String -> Textarea AndreSamtaleStegMsg
viewSammendragInput sammendrag =
    Textarea.textarea { label = "Sammendrag", msg = SammendragEndret } sammendrag
        |> Textarea.withTextAreaClass "textarea_stor"
        |> Textarea.withFeilmelding (Validering.feilmeldingMaxAntallTegn sammendrag 4000)
        |> Textarea.withId (inputIdTilString SammendragId)


viewBekreftSammendrag : BrukerInput AndreSamtaleStegMsg
viewBekreftSammendrag =
    BrukerInput.knapper Flytende
        [ Knapp.knapp VilLagreBekreftetSammendrag "Ja, jeg er fornøyd"
            |> Knapp.withId (inputIdTilString BekreftSammendragId)
        , Knapp.knapp BrukerVilEndreSammendrag "Nei, jeg vil endre"
        ]


seksjonsvalgKnapp : ValgtSeksjon -> Knapp AndreSamtaleStegMsg
seksjonsvalgKnapp seksjonsvalg =
    seksjonsvalg
        |> seksjonsvalgTilString
        |> Knapp.knapp (SeksjonValgt seksjonsvalg)


seksjonsvalgTilString : ValgtSeksjon -> String
seksjonsvalgTilString seksjonsvalg =
    case seksjonsvalg of
        FagbrevSvennebrevValgt ->
            "Fagbrev/svennebrev"

        MesterbrevValgt ->
            "Mesterbrev"

        SertifiseringValgt ->
            "Sertifisering/sertifikat"

        AnnenErfaringValgt ->
            "Annen erfaring"

        KursValgt ->
            "Kurs"

        FørerkortValgt ->
            "Førerkort"



--- PROGRAM ---


main =
    Browser.application
        { init = init
        , update = update
        , view = viewDocument
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequestChanged
        }


init : () -> Url.Url -> Navigation.Key -> ( ExtendedModel, Cmd Msg )
init _ url navigationKey =
    ( { model = Loading VenterPåPerson
      , windowWidth = 1000
      , navigationKey = navigationKey
      , debugStatus = DebugStatus.fromUrl url
      , modalStatus = ModalLukket
      , timeZone = Time.utc
      , time = Time.millisToPosix 0
      }
    , Cmd.batch
        [ Api.getPerson (PersonHentet >> LoadingMsg)
        , Dom.getViewport
            |> Task.perform ViewportHentet
        , Task.perform AdjustTimeZone Time.here
        ]
    )


subscriptions : ExtendedModel -> Sub Msg
subscriptions { model, debugStatus, modalStatus } =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Time.every (DebugStatus.tickInterval debugStatus) Tick
        , seksjonSubscriptions model
        , case modalStatus of
            ModalLukket ->
                Sub.none

            TilbakemeldingModalÅpen modalModel ->
                TilbakemeldingModal.subscriptions modalModel
                    |> Sub.map ModalMsg
        ]


seksjonSubscriptions : Model -> Sub Msg
seksjonSubscriptions model =
    case model of
        Loading _ ->
            Sub.none

        Failure _ ->
            Sub.none

        Success successModel ->
            case successModel.aktivSeksjon of
                PersonaliaSeksjon personaliaModel ->
                    personaliaModel
                        |> Personalia.Seksjon.subscriptions
                        |> Sub.map (PersonaliaMsg >> SuccessMsg)

                UtdanningSeksjon utdanningModel ->
                    utdanningModel
                        |> Utdanning.Seksjon.subscriptions
                        |> Sub.map (UtdanningsMsg >> SuccessMsg)

                ArbeidsErfaringSeksjon arbeidserfaringModel ->
                    arbeidserfaringModel
                        |> Arbeidserfaring.Seksjon.subscriptions
                        |> Sub.map (ArbeidserfaringsMsg >> SuccessMsg)

                SpråkSeksjon språkModel ->
                    språkModel
                        |> Sprak.Seksjon.subscriptions
                        |> Sub.map (SpråkMsg >> SuccessMsg)

                FagdokumentasjonSeksjon fagdokumentasjonModel ->
                    fagdokumentasjonModel
                        |> Fagdokumentasjon.Seksjon.subscriptions
                        |> Sub.map (FagdokumentasjonMsg >> SuccessMsg)

                SertifikatSeksjon sertifikatModel ->
                    sertifikatModel
                        |> Sertifikat.Seksjon.subscriptions
                        |> Sub.map (SertifikatMsg >> SuccessMsg)

                AnnenErfaringSeksjon annenErfaringModel ->
                    annenErfaringModel
                        |> AnnenErfaring.Seksjon.subscriptions
                        |> Sub.map (AnnenErfaringMsg >> SuccessMsg)

                FørerkortSeksjon førerkortModel ->
                    førerkortModel
                        |> Forerkort.Seksjon.subscriptions
                        |> Sub.map (FørerkortMsg >> SuccessMsg)

                KursSeksjon kursModel ->
                    kursModel
                        |> Kurs.Seksjon.subscriptions
                        |> Sub.map (KursMsg >> SuccessMsg)

                AndreSamtaleSteg info ->
                    Sub.batch
                        [ Browser.Events.onVisibilityChange (WindowEndrerVisibility >> AndreSamtaleStegMsg >> SuccessMsg)
                        , info.meldingsLogg
                            |> SamtaleAnimasjon.subscriptions
                            |> Sub.map (SamtaleAnimasjonMsg >> AndreSamtaleStegMsg >> SuccessMsg)
                        ]

                JobbprofilSeksjon jobbprofilModel ->
                    jobbprofilModel
                        |> Jobbprofil.Seksjon.subscriptions
                        |> Sub.map (JobbprofilMsg >> SuccessMsg)
