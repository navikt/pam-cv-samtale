module Main exposing (main)

import Api
import Arbeidserfaring.Seksjon
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Cv.Cv as Cv exposing (Cv)
import DebugStatus exposing (DebugStatus)
import Fagdokumentasjon.Seksjon
import Feilmelding
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Header as Header
import FrontendModuler.Knapp as Knapp
import FrontendModuler.RobotLogo as RobotLogo
import FrontendModuler.Spinner as Spinner
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsGruppe(..), MeldingsLogg, MeldingsPlassering(..), SkriveStatus(..))
import Person exposing (Person)
import Personalia exposing (Personalia)
import Personalia.Seksjon
import Process
import SamtaleAnimasjon
import Seksjon.Avslutning
import Seksjon.Sammendrag
import Seksjon.Seksjonsvalg
import Sertifikat.Seksjon
import Sprak.Seksjon
import Task
import Url
import Utdanning.Seksjon



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
    }


type Model
    = Loading LoadingModel
    | Success SuccessModel
    | Failure Http.Error


type LoadingModel
    = VenterPåPerson
    | VenterPåPersonalia
    | VenterPåResten LoadingState


type alias LoadingState =
    { cv : Maybe Cv
    , personalia : Personalia
    , registreringsProgresjon : Maybe RegistreringsProgresjon
    , windowWidth : Maybe Int
    }


type alias SuccessModel =
    { cv : Cv
    , personalia : Personalia
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
    | SeksjonsvalgSeksjon Seksjon.Seksjonsvalg.Model
    | AvslutningSeksjon Seksjon.Avslutning.Model
    | SammendragSeksjon Seksjon.Sammendrag.Model
    | AndreSamtaleSteg AndreSamtaleStegInfo


type alias AndreSamtaleStegInfo =
    { aktivSamtale : Samtale
    , meldingsLogg : MeldingsLogg
    }


type Samtale
    = Introduksjon



--- UPDATE ---


type Msg
    = LoadingMsg LoadingMsg
    | SuccessMsg SuccessMsg
    | ErrorLogget (Result Http.Error ())
    | ViewportHentet Dom.Viewport
    | WindowResized Int Int
    | UrlChanged Url.Url
    | UrlRequestChanged Browser.UrlRequest


type LoadingMsg
    = PersonHentet (Result Http.Error Person)
    | PersonOpprettet (Result Http.Error ())
    | PersonaliaHentet (Result Http.Error Personalia)
    | PersonaliaOpprettet (Result Http.Error Personalia)
    | CvHentet (Result Http.Error Cv)
    | CvOpprettet (Result Http.Error Cv)
    | RegistreringsProgresjonHentet (Result Http.Error RegistreringsProgresjon)


type SuccessMsg
    = ViewportSatt (Result Dom.Error ())
    | PersonaliaMsg Personalia.Seksjon.Msg
    | UtdanningsMsg Utdanning.Seksjon.Msg
    | ArbeidserfaringsMsg Arbeidserfaring.Seksjon.Msg
    | SpråkMsg Sprak.Seksjon.Msg
    | SeksjonsvalgMsg Seksjon.Seksjonsvalg.Msg
    | SammendragMsg Seksjon.Sammendrag.Msg
    | FagdokumentasjonMsg Fagdokumentasjon.Seksjon.Msg
    | SertifikatMsg Sertifikat.Seksjon.Msg
    | AvslutningMsg Seksjon.Avslutning.Msg
    | AndreSamtaleStegMsg AndreSamtaleStegMsg


type AndreSamtaleStegMsg
    = BrukerSierHeiIIntroduksjonen2
    | StartÅSkrive
    | FullførMelding


update : Msg -> ExtendedModel -> ( ExtendedModel, Cmd Msg )
update msg extendedModel =
    case msg of
        LoadingMsg loadingModel ->
            updateLoading extendedModel.debugStatus extendedModel.navigationKey loadingModel extendedModel.model
                |> mapTilExtendedModel extendedModel

        SuccessMsg successMsg ->
            case extendedModel.model of
                Success successModel ->
                    let
                        ( nyModel, successCmd ) =
                            updateSuccess successMsg successModel
                    in
                    ( nyModel, Cmd.map SuccessMsg successCmd )
                        |> mapTilExtendedModel extendedModel

                _ ->
                    ( extendedModel, Cmd.none )

        ErrorLogget _ ->
            ( extendedModel, Cmd.none )

        WindowResized windowWidth _ ->
            ( { model = extendedModel.model
              , navigationKey = extendedModel.navigationKey
              , debugStatus = extendedModel.debugStatus
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
              , windowWidth = round viewport.scene.width
              }
            , Cmd.none
            )


mapTilExtendedModel : ExtendedModel -> ( Model, Cmd Msg ) -> ( ExtendedModel, Cmd Msg )
mapTilExtendedModel extendedModel ( model, cmd ) =
    ( { model = model
      , windowWidth = extendedModel.windowWidth
      , debugStatus = extendedModel.debugStatus
      , navigationKey = extendedModel.navigationKey
      }
    , cmd
    )



--- Loading ---


updateLoading : DebugStatus -> Navigation.Key -> LoadingMsg -> Model -> ( Model, Cmd Msg )
updateLoading debugStatus navigationKey msg model =
    case msg of
        PersonHentet result ->
            case result of
                Ok _ ->
                    ( Loading VenterPåPersonalia, Api.getPersonalia (PersonaliaHentet >> LoadingMsg) )

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            ( model, Api.postPerson (PersonOpprettet >> LoadingMsg) )

                        Http.BadStatus 401 ->
                            ( model, redirectTilLogin navigationKey )

                        _ ->
                            ( Failure error
                            , logFeilmelding error "Hent Person"
                            )

        PersonOpprettet result ->
            case result of
                Ok _ ->
                    ( Loading VenterPåPersonalia, Api.getPersonalia (PersonaliaHentet >> LoadingMsg) )

                Err error ->
                    ( Failure error
                    , logFeilmelding error "Opprett Person"
                    )

        PersonaliaHentet result ->
            case result of
                Ok personalia ->
                    initVenterPåResten personalia

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            ( model, Api.postPersonalia (PersonaliaOpprettet >> LoadingMsg) )

                        _ ->
                            ( Failure error
                            , logFeilmelding error "Hent Personalia"
                            )

        PersonaliaOpprettet result ->
            case result of
                Ok personalia ->
                    initVenterPåResten personalia

                Err error ->
                    ( Failure error
                    , logFeilmelding error "Opprett Personalia"
                    )

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
                                    , logFeilmelding error "Hent CV"
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
                            , logFeilmelding error "Opprett CV"
                            )

                _ ->
                    ( model, Cmd.none )

        RegistreringsProgresjonHentet result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err error ->
                    ( Failure error
                    , logFeilmelding error "Hent registreringsprogresjon"
                    )


redirectTilLogin : Navigation.Key -> Cmd Msg
redirectTilLogin _ =
    Navigation.load "/cv-samtale/login"


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


modelFraLoadingState : DebugStatus -> LoadingState -> ( Model, Cmd Msg )
modelFraLoadingState debugStatus state =
    case ( state.cv, state.registreringsProgresjon ) of
        ( Just cv, Just registreringsProgresjon ) ->
            ( Success
                { cv = cv
                , personalia = state.personalia
                , registreringsProgresjon = registreringsProgresjon
                , aktivSeksjon = initialiserSamtale state.personalia
                , debugStatus = debugStatus
                }
            , 200
                |> DebugStatus.meldingsTimeout debugStatus
                |> Process.sleep
                |> Task.perform (always (SuccessMsg (AndreSamtaleStegMsg StartÅSkrive)))
            )

        _ ->
            ( Loading (VenterPåResten state), Cmd.none )


initialiserSamtale : Personalia -> SamtaleSeksjon
initialiserSamtale personalia =
    AndreSamtaleSteg
        { aktivSamtale = Introduksjon
        , meldingsLogg =
            MeldingsLogg.init
                |> MeldingsLogg.leggTilSpørsmål
                    [ Melding.spørsmål [ "Hei, " ++ (Personalia.fornavn personalia |> Maybe.withDefault "") ++ "! Jeg er roboten Cvert, og jeg kan hjelpe deg å lage en CV." ]
                    , Melding.spørsmål [ "Først skal du legge inn utdanning, arbeidserfaring, språk og sammendrag. Etter det kan du legge til kurs, fagbrev, sertifisering og førerkort." ]
                    , Melding.spørsmål [ "Husk at du ikke skal legge inn noe om helse, religion eller politiske oppfatning." ]
                    , Melding.spørsmål [ "Er du klar til å begynne?" ]
                    ]
        }


initVenterPåResten : Personalia -> ( Model, Cmd Msg )
initVenterPåResten personalia =
    ( Loading
        (VenterPåResten
            { cv = Nothing
            , personalia = personalia
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



--- Success ---


updateSuccess : SuccessMsg -> SuccessModel -> ( Model, Cmd SuccessMsg )
updateSuccess successMsg model =
    case successMsg of
        PersonaliaMsg msg ->
            case model.aktivSeksjon of
                PersonaliaSeksjon personaliaModel ->
                    case Personalia.Seksjon.update msg personaliaModel of
                        Personalia.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> PersonaliaSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map PersonaliaMsg cmd
                            )

                        Personalia.Seksjon.Ferdig personalia personaliaMeldingsLogg ->
                            gåTilUtdanning model personaliaMeldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        UtdanningsMsg msg ->
            case model.aktivSeksjon of
                UtdanningSeksjon utdanningModel ->
                    case Utdanning.Seksjon.update msg utdanningModel of
                        Utdanning.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> UtdanningSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map UtdanningsMsg cmd
                            )

                        Utdanning.Seksjon.Ferdig utdanning meldingsLogg ->
                            gåTilArbeidserfaring model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        ArbeidserfaringsMsg msg ->
            case model.aktivSeksjon of
                ArbeidsErfaringSeksjon arbeidserfaringsModel ->
                    case Arbeidserfaring.Seksjon.update msg arbeidserfaringsModel of
                        Arbeidserfaring.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> ArbeidsErfaringSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map ArbeidserfaringsMsg cmd
                            )

                        Arbeidserfaring.Seksjon.Ferdig ferdigAnimertMeldingsLogg ->
                            gåTilSpråk model ferdigAnimertMeldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        ViewportSatt _ ->
            ( Success model, Cmd.none )

        SpråkMsg msg ->
            case model.aktivSeksjon of
                SpråkSeksjon språkModel ->
                    case Sprak.Seksjon.update msg språkModel of
                        Sprak.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SpråkSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map SpråkMsg cmd
                            )

                        Sprak.Seksjon.Ferdig meldingsLogg ->
                            gåTilSeksjonsValg model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        SammendragMsg msg ->
            case model.aktivSeksjon of
                SammendragSeksjon sammendragModel ->
                    case Seksjon.Sammendrag.update msg sammendragModel of
                        Seksjon.Sammendrag.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SammendragSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map SammendragMsg cmd
                            )

                        Seksjon.Sammendrag.Ferdig meldingsLogg ->
                            gåTilAvslutning model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        AvslutningMsg msg ->
            case model.aktivSeksjon of
                AvslutningSeksjon avslutningModel ->
                    case Seksjon.Avslutning.update msg avslutningModel of
                        Seksjon.Avslutning.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> AvslutningSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map AvslutningMsg cmd
                            )

                        Seksjon.Avslutning.Ferdig nyModel meldingsLogg ->
                            ( Success model, Cmd.none )

                _ ->
                    ( Success model, Cmd.none )

        SeksjonsvalgMsg msg ->
            case model.aktivSeksjon of
                SeksjonsvalgSeksjon seksjonsvalgModel ->
                    case Seksjon.Seksjonsvalg.update msg seksjonsvalgModel of
                        Seksjon.Seksjonsvalg.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SeksjonsvalgSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map SeksjonsvalgMsg cmd
                            )

                        Seksjon.Seksjonsvalg.Ferdig seksjon meldingsLogg ->
                            -- TODO: her legger man inn caser for hvor seksjonsvalget skal gå
                            case seksjon of
                                -- FIXME: ikke implementert
                                Seksjon.Seksjonsvalg.FagbrevSvennebrevSeksjon ->
                                    gåTilFagbrev model meldingsLogg

                                Seksjon.Seksjonsvalg.MesterbrevSeksjon ->
                                    gåTilMesterbrev model meldingsLogg

                                Seksjon.Seksjonsvalg.AutorisasjonSeksjon ->
                                    gåTilAutorisasjon model meldingsLogg

                                Seksjon.Seksjonsvalg.SertifiseringSeksjon ->
                                    gåTilSertifisering model meldingsLogg

                                Seksjon.Seksjonsvalg.AnnenErfaringSeksjon ->
                                    ( Success model, Cmd.none )

                                Seksjon.Seksjonsvalg.KursSeksjon ->
                                    ( Success model, Cmd.none )

                                Seksjon.Seksjonsvalg.FørerkortSeksjon ->
                                    ( Success model, Cmd.none )

                                Seksjon.Seksjonsvalg.IngenAvSeksjonene ->
                                    gåTilSammendrag model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        FagdokumentasjonMsg msg ->
            case model.aktivSeksjon of
                FagdokumentasjonSeksjon fagdokumentasjonModel ->
                    case Fagdokumentasjon.Seksjon.update msg fagdokumentasjonModel of
                        Fagdokumentasjon.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> FagdokumentasjonSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map FagdokumentasjonMsg cmd
                            )

                        Fagdokumentasjon.Seksjon.Ferdig fagdokumentasjonListe meldingsLogg ->
                            gåTilFlereSeksjonsValg model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        SertifikatMsg msg ->
            case model.aktivSeksjon of
                SertifikatSeksjon sertifikatModel ->
                    case Sertifikat.Seksjon.update msg sertifikatModel of
                        Sertifikat.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SertifikatSeksjon
                                |> oppdaterSamtaleSteg model
                            , Cmd.map SertifikatMsg cmd
                            )

                        Sertifikat.Seksjon.Ferdig sertifikatListe meldingsLogg ->
                            gåTilFlereSeksjonsValg model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        AndreSamtaleStegMsg andreSamtaleStegMsg ->
            case model.aktivSeksjon of
                AndreSamtaleSteg andreSamtaleStegInfo ->
                    updateAndreSamtaleSteg model andreSamtaleStegMsg andreSamtaleStegInfo

                _ ->
                    ( Success model, Cmd.none )


updateAndreSamtaleSteg : SuccessModel -> AndreSamtaleStegMsg -> AndreSamtaleStegInfo -> ( Model, Cmd SuccessMsg )
updateAndreSamtaleSteg model msg info =
    case msg of
        BrukerSierHeiIIntroduksjonen2 ->
            case info.aktivSamtale of
                Introduksjon ->
                    let
                        ( personaliaModel, personaliaCmd ) =
                            info.meldingsLogg
                                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja!" ])
                                |> Personalia.Seksjon.init model.debugStatus model.personalia
                    in
                    ( personaliaModel
                        |> PersonaliaSeksjon
                        |> oppdaterSamtaleSteg model
                    , Cmd.map PersonaliaMsg personaliaCmd
                    )

        StartÅSkrive ->
            ( { info | meldingsLogg = MeldingsLogg.startÅSkrive info.meldingsLogg }
                |> AndreSamtaleSteg
                |> oppdaterSamtaleSteg model
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , MeldingsLogg.nesteMeldingToString info.meldingsLogg
                    * 1000.0
                    |> DebugStatus.meldingsTimeout model.debugStatus
                    |> Process.sleep
                    |> Task.perform (always (AndreSamtaleStegMsg FullførMelding))
                ]
            )

        FullførMelding ->
            let
                nyMeldingslogg =
                    MeldingsLogg.fullførMelding info.meldingsLogg
            in
            ( { info | meldingsLogg = nyMeldingslogg }
                |> AndreSamtaleSteg
                |> oppdaterSamtaleSteg model
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , case MeldingsLogg.ferdigAnimert nyMeldingslogg of
                    FerdigAnimert _ ->
                        Cmd.none

                    MeldingerGjenstår ->
                        200
                            |> DebugStatus.meldingsTimeout model.debugStatus
                            |> Process.sleep
                            |> Task.perform (always (AndreSamtaleStegMsg StartÅSkrive))
                ]
            )


oppdaterSamtaleSteg : SuccessModel -> SamtaleSeksjon -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Success { model | aktivSeksjon = samtaleSeksjon }


gåTilArbeidserfaring : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilArbeidserfaring model ferdigAnimertMeldingsLogg =
    let
        ( arbeidsModell, arbeidsCmd ) =
            Arbeidserfaring.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.arbeidserfaring model.cv)
    in
    ( Success { model | aktivSeksjon = ArbeidsErfaringSeksjon arbeidsModell }
    , Cmd.map ArbeidserfaringsMsg arbeidsCmd
    )


gåTilUtdanning : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilUtdanning model ferdigAnimertMeldingsLogg =
    let
        ( utdanningModel, utdanningCmd ) =
            Utdanning.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.utdanning model.cv)
    in
    ( Success
        { model
            | aktivSeksjon = UtdanningSeksjon utdanningModel
        }
    , Cmd.map UtdanningsMsg utdanningCmd
    )


gåTilSpråk : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilSpråk model ferdigAnimertMeldingsLogg =
    let
        ( språkModel, språkCmd ) =
            Sprak.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.spraakferdighet model.cv)
    in
    ( Success
        { model
            | aktivSeksjon = SpråkSeksjon språkModel
        }
    , Cmd.map SpråkMsg språkCmd
    )


gåTilFagbrev : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilFagbrev model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initFagbrev model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( Success
        { model
            | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
        }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilMesterbrev : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilMesterbrev model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initMesterbrev model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( Success
        { model
            | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
        }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilAutorisasjon : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilAutorisasjon model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initAutorisasjon model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( Success
        { model
            | aktivSeksjon = FagdokumentasjonSeksjon fagbrevModel
        }
    , Cmd.map FagdokumentasjonMsg fagbrevCmd
    )


gåTilSertifisering : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilSertifisering model ferdigAnimertMeldingsLogg =
    let
        ( sertifikatModel, sertifikatCmd ) =
            Sertifikat.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.sertifikater model.cv)
    in
    ( Success
        { model
            | aktivSeksjon = SertifikatSeksjon sertifikatModel
        }
    , Cmd.map SertifikatMsg sertifikatCmd
    )


gåTilSammendrag : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilSammendrag model ferdigAnimertMeldingsLogg =
    let
        ( sammendragModel, sammendragCmd ) =
            Seksjon.Sammendrag.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.sammendrag model.cv)
    in
    ( Success
        { model
            | aktivSeksjon = SammendragSeksjon sammendragModel
        }
    , Cmd.map SammendragMsg sammendragCmd
    )


gåTilAvslutning : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilAvslutning model ferdigAnimertMeldingsLogg =
    let
        ( avslutningModel, avslutningCmd ) =
            Seksjon.Avslutning.init model.debugStatus model.personalia model.cv ferdigAnimertMeldingsLogg
    in
    ( Success
        { model
            | aktivSeksjon = AvslutningSeksjon avslutningModel
        }
    , Cmd.map AvslutningMsg avslutningCmd
    )


gåTilSeksjonsValg : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilSeksjonsValg model ferdigAnimertMeldingsLogg =
    let
        ( seksjonsvalgModel, seksjonsvalgCmd ) =
            Seksjon.Seksjonsvalg.initLeggTil model.debugStatus ferdigAnimertMeldingsLogg
    in
    ( Success { model | aktivSeksjon = SeksjonsvalgSeksjon seksjonsvalgModel }
    , Cmd.map SeksjonsvalgMsg seksjonsvalgCmd
    )


gåTilFlereSeksjonsValg : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd SuccessMsg )
gåTilFlereSeksjonsValg model ferdigAnimertMeldingsLogg =
    let
        ( seksjonsvalgModel, seksjonsvalgCmd ) =
            Seksjon.Seksjonsvalg.initLeggTilFlere model.debugStatus ferdigAnimertMeldingsLogg
    in
    ( Success
        { model
            | aktivSeksjon = SeksjonsvalgSeksjon seksjonsvalgModel
        }
    , Cmd.map SeksjonsvalgMsg seksjonsvalgCmd
    )



--- VIEW ---


viewDocument : ExtendedModel -> Browser.Document Msg
viewDocument extendedModel =
    { title = "CV-samtale - Arbeidsplassen"
    , body = [ view extendedModel ]
    }


view : ExtendedModel -> Html Msg
view { model, windowWidth } =
    div [ class "app" ]
        [ Header.header windowWidth
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


viewLoading : Html msg
viewLoading =
    div [ class "spinner-wrapper" ]
        [ Spinner.spinner
            |> Spinner.withStørrelse Spinner.L
            |> Spinner.toHtml
        ]


meldingsLoggFraSeksjon : SuccessModel -> MeldingsLogg
meldingsLoggFraSeksjon successModel =
    case successModel.aktivSeksjon of
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

        SeksjonsvalgSeksjon model ->
            Seksjon.Seksjonsvalg.meldingsLogg model

        SammendragSeksjon model ->
            Seksjon.Sammendrag.meldingsLogg model

        AvslutningSeksjon model ->
            Seksjon.Avslutning.meldingsLogg model

        SertifikatSeksjon model ->
            Sertifikat.Seksjon.meldingsLogg model

        AndreSamtaleSteg andreSamtaleStegInfo ->
            andreSamtaleStegInfo.meldingsLogg


viewSuccess : SuccessModel -> Html Msg
viewSuccess successModel =
    div [ class "samtale-wrapper", id "samtale" ]
        [ div [ class "samtale" ]
            [ successModel
                |> meldingsLoggFraSeksjon
                |> viewMeldingsLogg
            , successModel
                |> meldingsLoggFraSeksjon
                |> viewSkriveStatus
            , viewBrukerInput successModel.aktivSeksjon
            , div [ class "samtale-padding" ] []
            ]
        ]


viewMeldingsLogg : MeldingsLogg -> Html Msg
viewMeldingsLogg meldingsLogg =
    meldingsLogg
        |> MeldingsLogg.mapMeldingsGruppe viewMeldingsgruppe
        |> div []


viewMeldingsgruppe : MeldingsGruppe -> Html msg
viewMeldingsgruppe meldingsGruppe =
    case meldingsGruppe of
        SpørsmålGruppe meldingsGruppeMeldinger ->
            meldingsGruppeMeldinger
                |> MeldingsLogg.mapMeldingsGruppeMeldinger (viewMelding "sporsmal")
                |> div [ class "meldingsgruppe" ]

        SvarGruppe meldingsGruppeMeldinger ->
            meldingsGruppeMeldinger
                |> MeldingsLogg.mapMeldingsGruppeMeldinger (viewMelding "svar")
                |> div [ class "meldingsgruppe" ]


viewMelding : String -> MeldingsPlassering -> Melding -> Html msg
viewMelding meldingsTypeClass plassering melding =
    div [ class ("meldingsrad " ++ meldingsTypeClass) ]
        [ case plassering of
            SisteSpørsmålIMeldingsgruppe ->
                div [ class "robot" ] [ RobotLogo.robotLogo ]

            IkkeSisteSpørsmål ->
                div [ class "robot" ] []
        , div [ class "melding" ]
            [ Melding.innhold melding
                |> List.map (\elem -> p [] [ text elem ])
                |> div []
            ]
        ]


viewSkriveStatus : MeldingsLogg -> Html msg
viewSkriveStatus meldingsLogg =
    case MeldingsLogg.skriveStatus meldingsLogg of
        MeldingsLogg.Skriver ->
            div [ class "meldingsrad sporsmal" ]
                [ div [ class "robot" ] [ RobotLogo.robotLogo ]
                , div [ class "melding" ]
                    [ div [ class "skriver-melding" ]
                        [ div [ class "bounce bounce1" ] []
                        , div [ class "bounce bounce2" ] []
                        , div [ class "bounce bounce3" ] []
                        ]
                    ]
                ]

        MeldingsLogg.SkriverIkke ->
            text ""


viewBrukerInput : SamtaleSeksjon -> Html Msg
viewBrukerInput aktivSamtale =
    case aktivSamtale of
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

        SammendragSeksjon sammendragSeksjon ->
            sammendragSeksjon
                |> Seksjon.Sammendrag.viewBrukerInput
                |> Html.map (SammendragMsg >> SuccessMsg)

        ArbeidsErfaringSeksjon arbeidserfaringSeksjon ->
            arbeidserfaringSeksjon
                |> Arbeidserfaring.Seksjon.viewBrukerInput
                |> Html.map (ArbeidserfaringsMsg >> SuccessMsg)

        FagdokumentasjonSeksjon fagbrevSeksjon ->
            fagbrevSeksjon
                |> Fagdokumentasjon.Seksjon.viewBrukerInput
                |> Html.map (FagdokumentasjonMsg >> SuccessMsg)

        AvslutningSeksjon avslutningSeksjon ->
            avslutningSeksjon
                |> Seksjon.Avslutning.viewBrukerInput
                |> Html.map (AvslutningMsg >> SuccessMsg)

        SeksjonsvalgSeksjon seksjonsvalgSeksjon ->
            seksjonsvalgSeksjon
                |> Seksjon.Seksjonsvalg.viewBrukerInput
                |> Html.map (SeksjonsvalgMsg >> SuccessMsg)

        SertifikatSeksjon sertifikatSeksjon ->
            sertifikatSeksjon
                |> Sertifikat.Seksjon.viewBrukerInput
                |> Html.map (SertifikatMsg >> SuccessMsg)

        AndreSamtaleSteg andreSamtaleStegInfo ->
            viewBrukerInputForAndreSamtaleSteg andreSamtaleStegInfo
                |> Html.map (AndreSamtaleStegMsg >> SuccessMsg)


viewBrukerInputForAndreSamtaleSteg : AndreSamtaleStegInfo -> Html AndreSamtaleStegMsg
viewBrukerInputForAndreSamtaleSteg info =
    case MeldingsLogg.ferdigAnimert info.meldingsLogg of
        FerdigAnimert _ ->
            case info.aktivSamtale of
                Introduksjon ->
                    Containers.knapper Flytende
                        [ Knapp.knapp BrukerSierHeiIIntroduksjonen2 "Ja!"
                            |> Knapp.toHtml
                        ]

        MeldingerGjenstår ->
            text ""



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
      }
    , Cmd.batch
        [ Api.getPerson (PersonHentet >> LoadingMsg)
        , Dom.getViewport
            |> Task.perform ViewportHentet
        ]
    )


subscriptions : ExtendedModel -> Sub Msg
subscriptions { model } =
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , seksjonSubscriptions model
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

                UtdanningSeksjon _ ->
                    Sub.none

                ArbeidsErfaringSeksjon _ ->
                    Sub.none

                SpråkSeksjon _ ->
                    Sub.none

                FagdokumentasjonSeksjon fagdokumentasjonModel ->
                    fagdokumentasjonModel
                        |> Fagdokumentasjon.Seksjon.subscriptions
                        |> Sub.map (FagdokumentasjonMsg >> SuccessMsg)

                SertifikatSeksjon _ ->
                    Sub.none

                SeksjonsvalgSeksjon _ ->
                    Sub.none

                AvslutningSeksjon _ ->
                    Sub.none

                SammendragSeksjon _ ->
                    Sub.none

                AndreSamtaleSteg andreSamtaleStegInfo ->
                    Sub.none
