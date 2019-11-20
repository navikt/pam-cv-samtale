module Main exposing (main)

import AnnenErfaring.Seksjon
import Api
import Arbeidserfaring.Seksjon
import Browser
import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..))
import Browser.Navigation as Navigation
import Cv.Cv as Cv exposing (Cv)
import Cv.Sammendrag as Sammendrag exposing (Sammendrag)
import DebugStatus exposing (DebugStatus)
import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Fagdokumentasjon.Seksjon
import Feilmelding
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Header as Header
import FrontendModuler.Knapp as Knapp exposing (Enabled(..))
import FrontendModuler.Lenke as Lenke
import FrontendModuler.LoggInnLenke as LoggInnLenke
import FrontendModuler.Spinner as Spinner
import FrontendModuler.Textarea as Textarea
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaHidden, ariaLabel, ariaLive)
import Http
import Konstanter
import Kurs.Seksjon
import LagreStatus exposing (LagreStatus)
import Melding exposing (Melding, Tekstområde(..))
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsGruppeViewState(..), MeldingsLogg, SpørsmålsGruppeViewState)
import Person exposing (Person)
import Personalia exposing (Personalia)
import Personalia.Seksjon
import SamtaleAnimasjon
import Sertifikat.Seksjon
import SporsmalViewState as SpørsmålViewState exposing (IkonStatus(..), SpørsmålStyle(..), SpørsmålViewState)
import Sprak.Seksjon
import Task
import TilbakemeldingModal
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
    | FokusSatt (Result Dom.Error ())


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
              }
            , Cmd.none
            )

        ÅpneTilbakemeldingModal ->
            let
                ( modalModel, cmd ) =
                    TilbakemeldingModal.init
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

        FokusSatt _ ->
            ( extendedModel, Cmd.none )


mapTilExtendedModel : ExtendedModel -> ( Model, Cmd Msg ) -> ( ExtendedModel, Cmd Msg )
mapTilExtendedModel extendedModel ( model, cmd ) =
    ( { model = model
      , windowWidth = extendedModel.windowWidth
      , debugStatus = extendedModel.debugStatus
      , navigationKey = extendedModel.navigationKey
      , modalStatus = extendedModel.modalStatus
      }
    , cmd
    )



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
                            ( model, Api.postPerson (PersonOpprettet >> LoadingMsg) )

                        Http.BadStatus 401 ->
                            ( model, redirectTilLogin )

                        _ ->
                            ( Failure error
                            , logFeilmeldingUnderLoading error "Hent Person"
                            )

        PersonOpprettet result ->
            case result of
                Ok person ->
                    personHentet model person

                Err error ->
                    ( Failure error
                    , logFeilmeldingUnderLoading error "Opprett Person"
                    )

        PersonaliaHentet result ->
            case model of
                Loading (VenterPåPersonalia person) ->
                    case result of
                        Ok personalia ->
                            initVenterPåResten person personalia

                        Err error ->
                            case error of
                                Http.BadStatus 404 ->
                                    ( model, Api.postPersonalia (PersonaliaOpprettet >> LoadingMsg) )

                                _ ->
                                    ( Failure error
                                    , logFeilmeldingUnderLoading error "Hent Personalia"
                                    )

                _ ->
                    ( model, Cmd.none )

        PersonaliaOpprettet result ->
            case model of
                Loading (VenterPåPersonalia person) ->
                    case result of
                        Ok personalia ->
                            initVenterPåResten person personalia

                        Err error ->
                            ( Failure error
                            , logFeilmeldingUnderLoading error "Opprett Personalia"
                            )

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
                                    ( model, Api.postCv (CvOpprettet >> LoadingMsg) )

                                _ ->
                                    ( Failure error
                                    , logFeilmeldingUnderLoading error "Hent CV"
                                    )

                _ ->
                    ( model, Cmd.none )

        CvOpprettet result ->
            case model of
                Loading (VenterPåResten state) ->
                    case result of
                        Ok cv ->
                            modelFraLoadingState debugStatus { state | cv = Just cv }

                        Err error ->
                            ( Failure error
                            , logFeilmeldingUnderLoading error "Opprett CV"
                            )

                _ ->
                    ( model, Cmd.none )

        RegistreringsProgresjonHentet result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err error ->
                    ( Failure error
                    , logFeilmeldingUnderLoading error "Hent registreringsprogresjon"
                    )


personHentet : Model -> Person -> ( Model, Cmd Msg )
personHentet model person =
    if Person.harGodtattVilkår person then
        ( Loading (VenterPåPersonalia person)
        , Api.getPersonalia (PersonaliaHentet >> LoadingMsg)
        )

    else
        ( model, redirectTilGodkjenningAvSamtykke )


redirectTilLogin : Cmd Msg
redirectTilLogin =
    Navigation.load "/cv-samtale/login"


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
                , aktivSeksjon = initialiserSamtale state.personalia
                , debugStatus = debugStatus
                }
            , lagtTilSpørsmålCmd debugStatus
                |> Cmd.map SuccessMsg
            )

        _ ->
            ( Loading (VenterPåResten state), Cmd.none )


initialiserSamtale : Personalia -> SamtaleSeksjon
initialiserSamtale personalia =
    let
        aktivSamtale =
            Introduksjon personalia
    in
    AndreSamtaleSteg
        { aktivSamtale = aktivSamtale
        , meldingsLogg =
            MeldingsLogg.init
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
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
    | KursSeksjon Kurs.Seksjon.Model
    | AndreSamtaleSteg AndreSamtaleStegInfo



--- SUCCESS UPDATE ---


type SuccessMsg
    = PersonaliaMsg Personalia.Seksjon.Msg
    | UtdanningsMsg Utdanning.Seksjon.Msg
    | ArbeidserfaringsMsg Arbeidserfaring.Seksjon.Msg
    | SpråkMsg Sprak.Seksjon.Msg
    | FagdokumentasjonMsg Fagdokumentasjon.Seksjon.Msg
    | SertifikatMsg Sertifikat.Seksjon.Msg
    | AnnenErfaringMsg AnnenErfaring.Seksjon.Msg
    | KursMsg Kurs.Seksjon.Msg
    | AndreSamtaleStegMsg AndreSamtaleStegMsg


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

                        Personalia.Seksjon.Ferdig personalia personaliaMeldingsLogg ->
                            gåTilUtdanning model personaliaMeldingsLogg

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

                        Utdanning.Seksjon.Ferdig utdanning meldingsLogg ->
                            gåTilArbeidserfaring model meldingsLogg

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

                        Arbeidserfaring.Seksjon.Ferdig ferdigAnimertMeldingsLogg ->
                            gåTilSpråk model ferdigAnimertMeldingsLogg

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

                        Sprak.Seksjon.Ferdig meldingsLogg ->
                            gåTilSeksjonsValg model meldingsLogg

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

                        Fagdokumentasjon.Seksjon.Ferdig fagdokumentasjonListe meldingsLogg ->
                            gåTilFlereSeksjonsValg model meldingsLogg

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

                        Sertifikat.Seksjon.Ferdig sertifikatListe meldingsLogg ->
                            gåTilFlereSeksjonsValg model meldingsLogg

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

                        AnnenErfaring.Seksjon.Ferdig _ meldingsLogg ->
                            gåTilFlereAnnetValg model meldingsLogg

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

                        Kurs.Seksjon.Ferdig _ meldingsLogg ->
                            gåTilFlereAnnetValg model meldingsLogg

                _ ->
                    ( model, Cmd.none )

        AndreSamtaleStegMsg andreSamtaleStegMsg ->
            case model.aktivSeksjon of
                AndreSamtaleSteg andreSamtaleStegInfo ->
                    updateAndreSamtaleSteg model andreSamtaleStegMsg andreSamtaleStegInfo

                _ ->
                    ( model, Cmd.none )


oppdaterSamtaleSeksjon : SuccessModel -> SamtaleSeksjon -> SuccessModel
oppdaterSamtaleSeksjon model samtaleSeksjon =
    { model | aktivSeksjon = samtaleSeksjon }


gåTilArbeidserfaring : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilArbeidserfaring model ferdigAnimertMeldingsLogg =
    let
        ( arbeidsModell, arbeidsCmd ) =
            Arbeidserfaring.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.arbeidserfaring model.cv)
    in
    ( { model | aktivSeksjon = ArbeidsErfaringSeksjon arbeidsModell }
    , Cmd.map ArbeidserfaringsMsg arbeidsCmd
    )


gåTilUtdanning : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilUtdanning model ferdigAnimertMeldingsLogg =
    let
        ( utdanningModel, utdanningCmd ) =
            Utdanning.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.utdanning model.cv)
    in
    ( { model
        | aktivSeksjon = UtdanningSeksjon utdanningModel
      }
    , Cmd.map UtdanningsMsg utdanningCmd
    )


gåTilSpråk : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilSpråk model ferdigAnimertMeldingsLogg =
    let
        ( språkModel, språkCmd ) =
            Sprak.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.spraakferdighet model.cv)
    in
    ( { model
        | aktivSeksjon = SpråkSeksjon språkModel
      }
    , Cmd.map SpråkMsg språkCmd
    )


gåTilFagbrev : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilFagbrev model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initFagbrev model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( { model
        | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
      }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilMesterbrev : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilMesterbrev model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initMesterbrev model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( { model
        | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
      }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilAutorisasjon : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilAutorisasjon model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initAutorisasjon model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( { model
        | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
      }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilSertifisering : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilSertifisering model ferdigAnimertMeldingsLogg =
    let
        ( sertifikatModel, sertifikatCmd ) =
            Sertifikat.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.sertifikater model.cv)
    in
    ( { model
        | aktivSeksjon = SertifikatSeksjon sertifikatModel
      }
    , Cmd.map SertifikatMsg sertifikatCmd
    )


gåTilAnnenErfaring : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilAnnenErfaring model ferdigAnimertMeldingsLogg =
    let
        ( annenErfaringModel, annenErfaringCmd ) =
            AnnenErfaring.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.annenErfaring model.cv)
    in
    ( { model
        | aktivSeksjon = AnnenErfaringSeksjon annenErfaringModel
      }
    , Cmd.map AnnenErfaringMsg annenErfaringCmd
    )


gåTilKurs : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilKurs model ferdigAnimertMeldingsLogg =
    let
        ( kursModel, kursCmd ) =
            Kurs.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.kurs model.cv)
    in
    ( { model
        | aktivSeksjon = KursSeksjon kursModel
      }
    , Cmd.map KursMsg kursCmd
    )


gåTilSeksjonsValg : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilSeksjonsValg model ferdigAnimertMeldingsLogg =
    ( { aktivSamtale = LeggTilAutorisasjoner
      , meldingsLogg =
            ferdigAnimertMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg LeggTilAutorisasjoner)
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )


gåTilFlereSeksjonsValg : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilFlereSeksjonsValg model ferdigAnimertMeldingsLogg =
    ( { aktivSamtale = LeggTilFlereAutorisasjoner
      , meldingsLogg =
            ferdigAnimertMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg LeggTilFlereAutorisasjoner)
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )


gåTilFlereAnnetValg : SuccessModel -> FerdigAnimertMeldingsLogg -> ( SuccessModel, Cmd SuccessMsg )
gåTilFlereAnnetValg model ferdigAnimertMeldingsLogg =
    ( { aktivSamtale = LeggTilFlereAnnet
      , meldingsLogg =
            ferdigAnimertMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg LeggTilFlereAnnet)
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )



--- ANDRE SAMTALESTEG MODEL ---


type alias AndreSamtaleStegInfo =
    { aktivSamtale : Samtale
    , meldingsLogg : MeldingsLogg
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
    | BekreftSammendrag BekreftSammendragState
    | SkriverSammendrag String
    | EndrerSammendrag String
    | LagrerSammendrag String LagreStatus
    | LagringAvSammendragFeilet Http.Error String
    | DelMedArbeidsgiver Bool
    | LagrerSynlighet Bool LagreStatus
    | LagringSynlighetFeilet Http.Error Bool
    | SpørOmTilbakemeldingIkkeUnderOppfølging
    | SpørOmTilbakemeldingUnderOppfølging
    | GiTilbakemelding
    | Avslutt Bool



--- ANDRE SAMTALESTEG UPDATE ---


type AndreSamtaleStegMsg
    = BrukerSierHeiIIntroduksjonen
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
    | BrukerGirOppÅLagre String
    | VilGiTilbakemelding
    | VilIkkeGiTilbakemelding
    | SynlighetPostet (Result Http.Error Bool)
    | WindowEndrerVisibility Visibility
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | ErrorLogget


type ValgtSeksjon
    = FagbrevSvennebrevValgt
    | MesterbrevValgt
    | AutorisasjonValgt
    | SertifiseringValgt
    | AnnenErfaringValgt
    | KursValgt


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
                                |> Personalia.Seksjon.init model.debugStatus model.personalia
                    in
                    ( personaliaModel
                        |> PersonaliaSeksjon
                        |> oppdaterSamtaleSeksjon model
                    , Cmd.map PersonaliaMsg personaliaCmd
                    )

                _ ->
                    ( model, Cmd.none )

        SeksjonValgt valgtSeksjon ->
            gåTilValgtSeksjon model info valgtSeksjon

        IngenAvAutorisasjonSeksjoneneValgt ->
            ( nesteSamtaleSteg model info (Melding.svar [ "Gå videre" ]) LeggTilAnnet
            , lagtTilSpørsmålCmd model.debugStatus
            )

        IngenAvDeAndreSeksjoneneValgt ->
            gåVidereFraSeksjonsvalg model info

        BrukerVilEndreSammendrag ->
            case info.aktivSamtale of
                BekreftSammendrag bekreftSammendragState ->
                    case bekreftSammendragState of
                        OpprinneligSammendrag sammendrag ->
                            ( sammendrag
                                |> Sammendrag.toString
                                |> EndrerSammendrag
                                |> nesteSamtaleSteg model info (Melding.svar [ "Nei, jeg vil endre" ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                        NyttSammendrag sammendrag ->
                            ( sammendrag
                                |> EndrerSammendrag
                                |> nesteSamtaleSteg model info (Melding.svar [ "Nei, jeg vil endre" ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                        EndretSammendrag sammendrag ->
                            ( sammendrag
                                |> EndrerSammendrag
                                |> nesteSamtaleSteg model info (Melding.svar [ "Nei, jeg vil endre" ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                _ ->
                    ( model, Cmd.none )

        SammendragEndret tekst ->
            case info.aktivSamtale of
                EndrerSammendrag _ ->
                    ( tekst
                        |> EndrerSammendrag
                        |> oppdaterSamtaleSteg model info
                    , Cmd.none
                    )

                SkriverSammendrag _ ->
                    ( tekst
                        |> SkriverSammendrag
                        |> oppdaterSamtaleSteg model info
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        VilLagreSammendragSkjema ->
            case info.aktivSamtale of
                EndrerSammendrag tekst ->
                    case Validering.feilmeldingMaxAntallTegn tekst 4000 of
                        Just _ ->
                            ( model, Cmd.none )

                        Nothing ->
                            ( BekreftSammendrag (EndretSammendrag tekst)
                                |> nesteSamtaleSteg model info (Melding.svar [ tekst ])
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                SkriverSammendrag tekst ->
                    case Validering.feilmeldingMaxAntallTegn tekst 4000 of
                        Just _ ->
                            ( model, Cmd.none )

                        Nothing ->
                            ( BekreftSammendrag (NyttSammendrag tekst)
                                |> nesteSamtaleSteg model info (Melding.svar [ tekst ])
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
                        |> nesteSamtaleSteg model info (Melding.svar [ "Prøv på nytt" ])
                    , Api.putSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) feiletSammendrag
                    )

                BekreftSammendrag bekreftSammendragState ->
                    case bekreftSammendragState of
                        OpprinneligSammendrag _ ->
                            { info | meldingsLogg = info.meldingsLogg |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja, jeg er fornøyd" ]) }
                                |> gåTilAvslutning model

                        NyttSammendrag sammendrag ->
                            ( LagreStatus.init
                                |> LagrerSammendrag sammendrag
                                |> nesteSamtaleSteg model info (Melding.svar [ "Ja, jeg er fornøyd" ])
                            , Api.putSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) sammendrag
                            )

                        EndretSammendrag sammendrag ->
                            ( LagreStatus.init
                                |> LagrerSammendrag sammendrag
                                |> nesteSamtaleSteg model info (Melding.svar [ "Ja, jeg er fornøyd" ])
                            , Api.putSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) sammendrag
                            )

                _ ->
                    ( model, Cmd.none )

        VilIkkeLagreSammendrag ->
            { info | meldingsLogg = info.meldingsLogg |> MeldingsLogg.leggTilSvar (Melding.svar [ "Gå videre uten å lagre" ]) }
                |> gåTilAvslutning model

        SammendragOppdatert result ->
            case info.aktivSamtale of
                LagrerSammendrag sammendrag lagreStatus ->
                    case result of
                        Ok _ ->
                            gåTilAvslutning model
                                { info
                                    | meldingsLogg =
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
                                        |> oppdaterSamtaleSteg model info
                                    , Api.putSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) sammendrag
                                    )

                                else
                                    ( LagringAvSammendragFeilet error sammendrag
                                        |> oppdaterSamtaleSteg model info
                                    , sammendrag
                                        |> Api.encodeSammendrag
                                        |> Api.logErrorWithRequestBody (AndreSamtaleStegMsg ErrorLogget) "Lagre sammendrag" error
                                    )

                            else
                                ( LagringAvSammendragFeilet error sammendrag
                                    |> nesteSamtaleStegUtenSvar model info
                                , Cmd.batch
                                    [ lagtTilSpørsmålCmd model.debugStatus
                                    , sammendrag
                                        |> Api.encodeSammendrag
                                        |> Api.logErrorWithRequestBody (AndreSamtaleStegMsg ErrorLogget) "Lagre sammendrag" error
                                    ]
                                )

                _ ->
                    ( model, Cmd.none )

        BrukerGodkjennerSynligCV ->
            ( LagreStatus.init
                |> LagrerSynlighet True
                |> nesteSamtaleSteg model info (Melding.svar [ "Ja, CV-en skal være synlig for arbeidsgivere" ])
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.postSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) True
                ]
            )

        BrukerGodkjennerIkkeSynligCV ->
            ( LagreStatus.init
                |> LagrerSynlighet False
                |> nesteSamtaleSteg model info (Melding.svar [ "Nei, CV-en skal bare være synlig for meg" ])
            , Cmd.batch
                [ lagtTilSpørsmålCmd model.debugStatus
                , Api.postSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) False
                ]
            )

        VilGiTilbakemelding ->
            ( GiTilbakemelding
                |> nesteSamtaleSteg model info (Melding.svar [ "Ja, jeg vil svare" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )

        VilIkkeGiTilbakemelding ->
            ( Avslutt False
                |> nesteSamtaleSteg model info (Melding.svar [ "Nei, jeg vil ikke svare" ])
            , lagtTilSpørsmålCmd model.debugStatus
            )

        SynlighetPostet result ->
            case info.aktivSamtale of
                LagrerSynlighet skalVæreSynlig lagreStatus ->
                    case result of
                        Ok _ ->
                            ( if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                SpørOmTilbakemeldingIkkeUnderOppfølging
                                    |> nesteSamtaleSteg model info (Melding.svar [ LoggInnLenke.loggInnLenkeTekst ])

                              else
                                nesteSamtaleStegUtenSvar model info SpørOmTilbakemeldingIkkeUnderOppfølging
                            , lagtTilSpørsmålCmd model.debugStatus
                            )

                        Err error ->
                            if LagreStatus.lagrerEtterUtlogging lagreStatus then
                                if LagreStatus.forsøkPåNytt lagreStatus then
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSynlighet skalVæreSynlig
                                        |> oppdaterSamtaleSteg model info
                                    , Api.postSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) skalVæreSynlig
                                    )

                                else
                                    ( skalVæreSynlig
                                        |> LagringSynlighetFeilet error
                                        |> oppdaterSamtaleSteg model info
                                    , error
                                        |> Feilmelding.feilmelding "Lagre synlighet"
                                        |> Maybe.map (Api.logError (always ErrorLogget >> AndreSamtaleStegMsg))
                                        |> Maybe.withDefault Cmd.none
                                    )

                            else
                                ( skalVæreSynlig
                                    |> LagringSynlighetFeilet error
                                    |> nesteSamtaleStegUtenSvar model info
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
                        |> oppdaterSamtaleSteg model info
                    , Api.postSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) skalVæreSynlig
                    )

                _ ->
                    ( model, Cmd.none )

        BrukerGirOppÅLagre knappeTekst ->
            ( nesteSamtaleSteg model info (Melding.svar [ knappeTekst ]) SpørOmTilbakemeldingIkkeUnderOppfølging
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
                                        |> oppdaterSamtaleSteg model info
                                    , Api.putSammendrag (SammendragOppdatert >> AndreSamtaleStegMsg) feiletSammendrag
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        LagrerSammendrag sammendrag lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSammendrag sammendrag
                                |> oppdaterSamtaleSteg model info
                            , Cmd.none
                            )

                        LagringSynlighetFeilet error skalVæreSynlig ->
                            case ErrorHåndtering.operasjonEtterError error of
                                LoggInn ->
                                    ( error
                                        |> LagreStatus.fraError
                                        |> LagrerSynlighet skalVæreSynlig
                                        |> oppdaterSamtaleSteg model info
                                    , Api.postSynlighet (SynlighetPostet >> AndreSamtaleStegMsg) skalVæreSynlig
                                    )

                                _ ->
                                    ( model, Cmd.none )

                        LagrerSynlighet skalVæreSynlig lagreStatus ->
                            ( lagreStatus
                                |> LagreStatus.setForsøkPåNytt
                                |> LagrerSynlighet skalVæreSynlig
                                |> oppdaterSamtaleSteg model info
                            , Cmd.none
                            )

                        GiTilbakemelding ->
                            ( Avslutt True
                                |> nesteSamtaleStegUtenSvar model info
                            , lagtTilSpørsmålCmd model.debugStatus
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
            ( { info | meldingsLogg = nyMeldingslogg }
                |> AndreSamtaleSteg
                |> oppdaterSamtaleSeksjon model
            , cmd
                |> Cmd.map (SamtaleAnimasjonMsg >> AndreSamtaleStegMsg)
            )

        ErrorLogget ->
            ( model, Cmd.none )


gåTilValgtSeksjon : SuccessModel -> AndreSamtaleStegInfo -> ValgtSeksjon -> ( SuccessModel, Cmd SuccessMsg )
gåTilValgtSeksjon model info valgtSeksjon =
    let
        meldingsLogg =
            info.meldingsLogg
                |> MeldingsLogg.leggTilSvar (Melding.svar [ seksjonsvalgTilString valgtSeksjon ])
    in
    case MeldingsLogg.ferdigAnimert meldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            case valgtSeksjon of
                FagbrevSvennebrevValgt ->
                    gåTilFagbrev model ferdigAnimertMeldingsLogg

                MesterbrevValgt ->
                    gåTilMesterbrev model ferdigAnimertMeldingsLogg

                AutorisasjonValgt ->
                    gåTilAutorisasjon model ferdigAnimertMeldingsLogg

                SertifiseringValgt ->
                    gåTilSertifisering model ferdigAnimertMeldingsLogg

                AnnenErfaringValgt ->
                    gåTilAnnenErfaring model ferdigAnimertMeldingsLogg

                KursValgt ->
                    gåTilKurs model ferdigAnimertMeldingsLogg

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
                        SkriverSammendrag ""

                    else
                        BekreftSammendrag (OpprinneligSammendrag sammendrag)

                Nothing ->
                    EndrerSammendrag ""
    in
    ( { meldingsLogg =
            info.meldingsLogg
                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Nei, gå videre" ])
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtale)
      , aktivSamtale = samtale
      }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model
    , lagtTilSpørsmålCmd model.debugStatus
    )


gåTilAvslutning : SuccessModel -> AndreSamtaleStegInfo -> ( SuccessModel, Cmd SuccessMsg )
gåTilAvslutning model info =
    if Person.underOppfolging model.person then
        ( nesteSamtaleStegUtenSvar model info SpørOmTilbakemeldingUnderOppfølging
        , lagtTilSpørsmålCmd model.debugStatus
        )

    else
        ( model.person
            |> Person.cvSynligForArbeidsgiver
            |> DelMedArbeidsgiver
            |> nesteSamtaleStegUtenSvar model info
        , lagtTilSpørsmålCmd model.debugStatus
        )


nesteSamtaleSteg : SuccessModel -> AndreSamtaleStegInfo -> Melding -> Samtale -> SuccessModel
nesteSamtaleSteg model info melding aktivSamtale =
    { info
        | aktivSamtale = aktivSamtale
        , meldingsLogg =
            info.meldingsLogg
                |> MeldingsLogg.leggTilSvar melding
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
    }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model


nesteSamtaleStegUtenSvar : SuccessModel -> AndreSamtaleStegInfo -> Samtale -> SuccessModel
nesteSamtaleStegUtenSvar model info aktivSamtale =
    { info
        | aktivSamtale = aktivSamtale
        , meldingsLogg =
            info.meldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
    }
        |> AndreSamtaleSteg
        |> oppdaterSamtaleSeksjon model


oppdaterSamtaleSteg : SuccessModel -> AndreSamtaleStegInfo -> Samtale -> SuccessModel
oppdaterSamtaleSteg model info aktivSamtale =
    { info | aktivSamtale = aktivSamtale }
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
            [ Melding.spørsmål [ "Vil du legge til flere kategorier?" ] ]

        LeggTilAnnet ->
            [ Melding.spørsmål [ "Det er viktig å få med alt du kan på CV-en." ]
            , Melding.spørsmål [ "Har du jobbet som frivillig eller har hatt verv? Legg til annen erfaring." ]
            , Melding.spørsmål [ "Har du tatt norskprøve? Legg til kurs." ]
            , Melding.spørsmål [ "Vil du legge til noen av disse kategoriene?" ]
            ]

        LeggTilFlereAnnet ->
            [ Melding.spørsmål [ "Vil du legge til flere kategorier?" ] ]

        BekreftSammendrag bekreftState ->
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

        EndrerSammendrag _ ->
            [ Melding.spørsmål [ "Gjør endringene du ønsker." ] ]

        SkriverSammendrag _ ->
            [ Melding.spørsmål [ "Supert, nå er vi snart ferdig med CV-en." ]
            , Melding.spørsmål [ "Nå skal du skrive et sammendrag. Her har du mulighet til å selge deg inn. Fortell arbeidsgivere om kompetansen din og personlige egenskaper." ]
            , Melding.spørsmål [ "Skriv sammendraget ditt i boksen under." ]
            ]

        LagrerSammendrag _ _ ->
            []

        LagringAvSammendragFeilet error _ ->
            [ ErrorHåndtering.errorMelding { error = error, operasjon = "lagre sammendrag" } ]

        DelMedArbeidsgiver synlig ->
            [ Melding.spørsmål [ "Du kan velge om arbeidsgivere skal få se CV-en din. Da kan de ta kontakt hvis de har en jobb du kan passe til. " ]
            , if synlig then
                Melding.spørsmål
                    [ "CV-en din er allerede synlig for arbeidsgivere!"
                    , "Ønsker du fremdeles at arbeidsgivere skal kunne se CV-en din?"
                    ]

              else
                Melding.spørsmål
                    [ "Ønsker du at arbeidsgivere skal kunne se CV-en din?" ]
            ]

        SpørOmTilbakemeldingUnderOppfølging ->
            [ Melding.spørsmål [ "Arbeidsgivere og NAV-veiledere kan søke opp CV-en din. De kan kontakte deg hvis de har en jobb som passer for deg." ]
            , Melding.spørsmål [ "CV-en din er synlig for arbeidsgivere og NAV-veiledere fordi du får oppfølging fra NAV." ]
            , Melding.spørsmål [ "Da er vi ferdige med CV-en. Husk at du når som helst kan endre og forbedre den." ]
            , Melding.spørsmål [ "Hvis du har tid, vil jeg gjerne vite hvordan du synes det var å lage CV-en. Du kan svare på 3 spørsmål, og du er anonym 😊 Vil du svare (det er frivillig)?" ]
            ]

        SpørOmTilbakemeldingIkkeUnderOppfølging ->
            [ Melding.spørsmål [ "Bra innsats! 👍👍 Alt du har lagt inn er nå lagret i CV-en din." ]
            , Melding.spørsmål [ "Da er vi ferdige med CV-en. Husk at du når som helst kan endre og forbedre den." ]
            , Melding.spørsmål [ "Hvis du har tid, vil jeg gjerne vite hvordan du synes det var å lage CV-en. Du kan svare på 3 spørsmål, og du er anonym 😊 Vil du svare (det er frivillig)?" ]
            ]

        GiTilbakemelding ->
            [ Melding.spørsmål [ "Så bra at du vil svare! Klikk på lenken." ]
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



--- VIEW ---


viewDocument : ExtendedModel -> Browser.Document Msg
viewDocument extendedModel =
    { title = "CV-samtale - Arbeidsplassen"
    , body = [ view extendedModel ]
    }


view : ExtendedModel -> Html Msg
view { model, windowWidth, modalStatus } =
    div [ class "app" ]
        [ case modalStatus of
            TilbakemeldingModalÅpen typeaheadModel ->
                typeaheadModel
                    |> TilbakemeldingModal.view
                    |> Html.map ModalMsg

            ModalLukket ->
                text ""
        , div [ ariaHidden (modalStatus /= ModalLukket) ]
            [ Header.header windowWidth ÅpneTilbakemeldingModal
                |> Header.toHtml
            , case model of
                Loading _ ->
                    viewLoading

                Success successModel ->
                    viewSuccess successModel

                Failure error ->
                    case error of
                        Http.BadUrl string ->
                            div []
                                [ text ("Fant ingenting her: " ++ string)
                                , text "Er du sikker på at du leter på riktig sted?"
                                ]

                        Http.Timeout ->
                            div []
                                [ text "Forespørselen tok for lang tid. Det kan være noe feil hos oss."
                                , text "Forsæk å laste inn siden på nytt eller prøv gjerne igen senere"
                                ]

                        Http.BadStatus int ->
                            div []
                                [ text ("Fikk en " ++ String.fromInt int ++ " feilmelding. Vennligst prøv igjen senere!")
                                ]

                        Http.BadBody _ ->
                            div []
                                [ text "Det set ut til at du ikke har godkjent vilkårene på arbeidsplassen.no/cv."
                                , text "Vennligst gjøre dette før du benytter det av tjenesten."
                                ]

                        _ ->
                            div []
                                [ text "error"
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

        KursSeksjon model ->
            Kurs.Seksjon.meldingsLogg model

        AndreSamtaleSteg andreSamtaleStegInfo ->
            andreSamtaleStegInfo.meldingsLogg


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


viewMeldingsLogg : MeldingsLogg -> Html Msg
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
    div [ class "meldingsrad sporsmal" ]
        [ div [ class "robot", robotAttribute spørsmål ]
            [ i [ class "Robotlogo" ] [] ]
        , case SpørsmålViewState.spørsmålStyle spørsmål of
            FørSkriveindikator ->
                div
                    [ class "melding skjult"
                    , ariaLive "off"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ viewSkriveStatus ]

            Skriveindikator ->
                div
                    [ class "melding skriveindikator"
                    , ariaLive "off"
                    , id (SpørsmålViewState.id spørsmål)
                    ]
                    [ viewSkriveStatus ]

            StørrelseKalkuleres ->
                article
                    [ class "melding kalkulerer"
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
                    [ class "melding ferdiganimert"
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
                    [ class "melding"
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

        KursSeksjon kursSeksjon ->
            kursSeksjon
                |> Kurs.Seksjon.viewBrukerInput
                |> Html.map (KursMsg >> SuccessMsg)

        AndreSamtaleSteg andreSamtaleStegInfo ->
            viewBrukerInputForAndreSamtaleSteg andreSamtaleStegInfo
                |> Html.map (AndreSamtaleStegMsg >> SuccessMsg)


viewBrukerInputForAndreSamtaleSteg : AndreSamtaleStegInfo -> Html AndreSamtaleStegMsg
viewBrukerInputForAndreSamtaleSteg info =
    if MeldingsLogg.visBrukerInput info.meldingsLogg then
        case info.aktivSamtale of
            Introduksjon _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerSierHeiIIntroduksjonen "Ja!"
                        |> Knapp.toHtml
                    ]

            BekreftSammendrag bekreftSammendragState ->
                case bekreftSammendragState of
                    OpprinneligSammendrag _ ->
                        viewBekreftSammendrag

                    NyttSammendrag _ ->
                        viewBekreftSammendrag

                    EndretSammendrag _ ->
                        viewBekreftSammendrag

            EndrerSammendrag sammendrag ->
                Containers.skjema { lagreMsg = VilLagreSammendragSkjema, lagreKnappTekst = "Lagre endringer" }
                    (viewSammendragInput sammendrag)

            SkriverSammendrag sammendrag ->
                Containers.inputMedGåVidereKnapp VilLagreSammendragSkjema
                    (viewSammendragInput sammendrag)

            LagrerSammendrag _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    text ""

            LagringAvSammendragFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        Containers.knapper Flytende
                            [ Knapp.knapp VilIkkeLagreSammendrag "Gå videre uten å lagre"
                                |> Knapp.toHtml
                            ]

                    PrøvPåNytt ->
                        Containers.knapper Flytende
                            [ Knapp.knapp VilLagreBekreftetSammendrag "Prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp VilIkkeLagreSammendrag "Gå videre uten å lagre"
                                |> Knapp.toHtml
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

            DelMedArbeidsgiver _ ->
                Containers.knapper Flytende
                    [ Knapp.knapp BrukerGodkjennerSynligCV "Ja, CV-en skal være synlig for arbeidsgivere"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerGodkjennerIkkeSynligCV "Nei, CV-en skal bare være synlig for meg"
                        |> Knapp.toHtml
                    ]

            SpørOmTilbakemeldingUnderOppfølging ->
                viewSpørOmTilbakemelding

            SpørOmTilbakemeldingIkkeUnderOppfølging ->
                viewSpørOmTilbakemelding

            GiTilbakemelding ->
                Containers.lenke
                    (Lenke.lenke { tekst = "Gi tilbakemelding", url = "https://surveys.hotjar.com/s?siteId=118350&surveyId=144585" }
                        |> Lenke.withTargetBlank
                        |> Lenke.toHtml
                    )

            Avslutt _ ->
                Containers.knapper Flytende
                    [ a [ href "/cv/forhandsvis", class "avslutt-knapp" ]
                        [ div [ class "Knapp" ]
                            [ text "Avslutt og vis CV-en min" ]
                        ]
                    ]

            LagrerSynlighet _ lagreStatus ->
                if LagreStatus.lagrerEtterUtlogging lagreStatus then
                    LoggInnLenke.viewLoggInnLenke

                else
                    text ""

            LagringSynlighetFeilet error _ ->
                case ErrorHåndtering.operasjonEtterError error of
                    GiOpp ->
                        Containers.knapper Flytende
                            [ Knapp.knapp (BrukerGirOppÅLagre "Gå videre") "Gå videre"
                                |> Knapp.toHtml
                            ]

                    PrøvPåNytt ->
                        Containers.knapper Flytende
                            [ Knapp.knapp BrukerVilPrøveÅLagreSynlighetPåNytt "Prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp (BrukerGirOppÅLagre "Gå videre") "Gå videre"
                                |> Knapp.toHtml
                            ]

                    LoggInn ->
                        LoggInnLenke.viewLoggInnLenke

    else
        text ""


sammendragId : String
sammendragId =
    "sammendrag-input"


viewLeggTilAutorisasjoner : Html AndreSamtaleStegMsg
viewLeggTilAutorisasjoner =
    Containers.knapper Kolonne
        [ seksjonsvalgKnapp FagbrevSvennebrevValgt
        , seksjonsvalgKnapp MesterbrevValgt
        , seksjonsvalgKnapp AutorisasjonValgt
        , seksjonsvalgKnapp SertifiseringValgt
        , Knapp.knapp IngenAvAutorisasjonSeksjoneneValgt "Nei, gå videre"
            |> Knapp.toHtml
        ]


viewLeggTilAnnet : Html AndreSamtaleStegMsg
viewLeggTilAnnet =
    Containers.knapper Kolonne
        [ seksjonsvalgKnapp AnnenErfaringValgt
        , seksjonsvalgKnapp KursValgt
        , Knapp.knapp IngenAvDeAndreSeksjoneneValgt "Nei, gå videre"
            |> Knapp.toHtml
        ]


viewSpørOmTilbakemelding : Html AndreSamtaleStegMsg
viewSpørOmTilbakemelding =
    Containers.knapper Flytende
        [ Knapp.knapp VilGiTilbakemelding "Ja, jeg vil svare"
            |> Knapp.toHtml
        , Knapp.knapp VilIkkeGiTilbakemelding "Nei, jeg vil ikke svare"
            |> Knapp.toHtml
        ]


viewSammendragInput : String -> List (Html AndreSamtaleStegMsg)
viewSammendragInput sammendrag =
    [ Textarea.textarea { label = "Sammendrag", msg = SammendragEndret } sammendrag
        |> Textarea.withTextAreaClass "textarea_stor"
        |> Textarea.withMaybeFeilmelding (Validering.feilmeldingMaxAntallTegn sammendrag 4000)
        |> Textarea.withId sammendragId
        |> Textarea.toHtml
    ]


viewBekreftSammendrag : Html AndreSamtaleStegMsg
viewBekreftSammendrag =
    Containers.knapper Flytende
        [ Knapp.knapp VilLagreBekreftetSammendrag "Ja, jeg er fornøyd"
            |> Knapp.toHtml
        , Knapp.knapp BrukerVilEndreSammendrag "Nei, jeg vil endre"
            |> Knapp.toHtml
        ]


seksjonsvalgKnapp : ValgtSeksjon -> Html AndreSamtaleStegMsg
seksjonsvalgKnapp seksjonsvalg =
    seksjonsvalg
        |> seksjonsvalgTilString
        |> Knapp.knapp (SeksjonValgt seksjonsvalg)
        |> Knapp.toHtml


seksjonsvalgTilString : ValgtSeksjon -> String
seksjonsvalgTilString seksjonsvalg =
    case seksjonsvalg of
        FagbrevSvennebrevValgt ->
            "Fagbrev/svennebrev"

        MesterbrevValgt ->
            "Mesterbrev"

        AutorisasjonValgt ->
            "Autorisasjon"

        SertifiseringValgt ->
            "Sertifisering/sertifikat"

        AnnenErfaringValgt ->
            "Annen erfaring"

        KursValgt ->
            "Kurs"



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
      }
    , Cmd.batch
        [ Api.getPerson (PersonHentet >> LoadingMsg)
        , Dom.getViewport
            |> Task.perform ViewportHentet
        ]
    )


subscriptions : ExtendedModel -> Sub Msg
subscriptions { model, modalStatus } =
    Sub.batch
        [ Browser.Events.onResize WindowResized
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
