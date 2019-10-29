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
import Forerkort.Seksjon
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
    , aktivSamtale : SamtaleSeksjon
    , debugStatus : DebugStatus
    }


type SamtaleSeksjon
    = Introduksjon MeldingsLogg
    | PersonaliaSeksjon Personalia.Seksjon.Model
    | UtdanningSeksjon Utdanning.Seksjon.Model
    | ArbeidsErfaringSeksjon Arbeidserfaring.Seksjon.Model
    | SpråkSeksjon Sprak.Seksjon.Model
    | FagdokumentasjonSeksjon Fagdokumentasjon.Seksjon.Model
    | SertifikatSeksjon Sertifikat.Seksjon.Model
    | SeksjonsvalgSeksjon Seksjon.Seksjonsvalg.Model
    | AvslutningSeksjon Seksjon.Avslutning.Model
    | SammendragSeksjon Seksjon.Sammendrag.Model
    | FørerkortSeksjon Forerkort.Seksjon.Model



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
    = BrukerSierHeiIIntroduksjonen
    | ViewportSatt (Result Dom.Error ())
    | PersonaliaMsg Personalia.Seksjon.Msg
    | UtdanningsMsg Utdanning.Seksjon.Msg
    | ArbeidserfaringsMsg Arbeidserfaring.Seksjon.Msg
    | SpråkMsg Sprak.Seksjon.Msg
    | FørerkortMsg Forerkort.Seksjon.Msg
    | SeksjonsvalgMsg Seksjon.Seksjonsvalg.Msg
    | SammendragMsg Seksjon.Sammendrag.Msg
    | FagdokumentasjonMsg Fagdokumentasjon.Seksjon.Msg
    | SertifikatMsg Sertifikat.Seksjon.Msg
    | AvslutningMsg Seksjon.Avslutning.Msg
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
                    updateSuccess successMsg successModel
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
                , aktivSamtale = initialiserSamtale state.personalia
                , debugStatus = debugStatus
                }
            , 200
                |> DebugStatus.meldingsTimeout debugStatus
                |> Process.sleep
                |> Task.perform (always (SuccessMsg StartÅSkrive))
            )

        _ ->
            ( Loading (VenterPåResten state), Cmd.none )


initialiserSamtale : Personalia -> SamtaleSeksjon
initialiserSamtale personalia =
    MeldingsLogg.init
        |> MeldingsLogg.leggTilSpørsmål
            [ Melding.spørsmål [ "Hei, " ++ (Personalia.fornavn personalia |> Maybe.withDefault "") ++ "! Jeg er roboten Cvert, og jeg kan hjelpe deg å lage en CV." ]
            , Melding.spørsmål [ "Først skal du legge inn utdanning, arbeidserfaring, språk og sammendrag. Etter det kan du legge til kurs, fagbrev, sertifisering og førerkort." ]
            , Melding.spørsmål [ "Husk at du ikke skal legge inn noe om helse, religion eller politiske oppfatning." ]
            , Melding.spørsmål [ "Er du klar til å begynne?" ]
            ]
        |> Introduksjon


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


updateSuccess : SuccessMsg -> SuccessModel -> ( Model, Cmd Msg )
updateSuccess successMsg model =
    case successMsg of
        BrukerSierHeiIIntroduksjonen ->
            case model.aktivSamtale of
                Introduksjon meldingsLogg ->
                    let
                        ( personaliaModel, personaliaCmd ) =
                            meldingsLogg
                                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja!" ])
                                |> Personalia.Seksjon.init model.debugStatus model.personalia
                    in
                    ( personaliaModel
                        |> PersonaliaSeksjon
                        |> oppdaterSamtaleSteg model
                        |> Success
                    , Cmd.map (PersonaliaMsg >> SuccessMsg) personaliaCmd
                    )

                _ ->
                    ( Success model, Cmd.none )

        PersonaliaMsg msg ->
            case model.aktivSamtale of
                PersonaliaSeksjon personaliaModel ->
                    case Personalia.Seksjon.update msg personaliaModel of
                        Personalia.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> PersonaliaSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (PersonaliaMsg >> SuccessMsg) cmd
                            )

                        Personalia.Seksjon.Ferdig personalia personaliaMeldingsLogg ->
                            gåTilUtdanning model personaliaMeldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        UtdanningsMsg msg ->
            case model.aktivSamtale of
                UtdanningSeksjon utdanningModel ->
                    case Utdanning.Seksjon.update msg utdanningModel of
                        Utdanning.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> UtdanningSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (UtdanningsMsg >> SuccessMsg) cmd
                            )

                        Utdanning.Seksjon.Ferdig utdanning meldingsLogg ->
                            gåTilArbeidserfaring model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        ArbeidserfaringsMsg msg ->
            case model.aktivSamtale of
                ArbeidsErfaringSeksjon arbeidserfaringsModel ->
                    case Arbeidserfaring.Seksjon.update msg arbeidserfaringsModel of
                        Arbeidserfaring.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> ArbeidsErfaringSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (ArbeidserfaringsMsg >> SuccessMsg) cmd
                            )

                        Arbeidserfaring.Seksjon.Ferdig ferdigAnimertMeldingsLogg ->
                            gåTilSpråk model ferdigAnimertMeldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        ViewportSatt _ ->
            ( Success model, Cmd.none )

        StartÅSkrive ->
            case model.aktivSamtale of
                Introduksjon meldingsLogg ->
                    ( meldingsLogg
                        |> MeldingsLogg.startÅSkrive
                        |> Introduksjon
                        |> oppdaterSamtaleSteg model
                        |> Success
                    , Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn (ViewportSatt >> SuccessMsg)
                        , MeldingsLogg.nesteMeldingToString meldingsLogg
                            * 1000.0
                            |> DebugStatus.meldingsTimeout model.debugStatus
                            |> Process.sleep
                            |> Task.perform (always (SuccessMsg FullførMelding))
                        ]
                    )

                _ ->
                    ( Success model, Cmd.none )

        FullførMelding ->
            case model.aktivSamtale of
                Introduksjon meldingsLogg ->
                    let
                        nyMeldingslogg =
                            MeldingsLogg.fullførMelding meldingsLogg
                    in
                    ( nyMeldingslogg
                        |> Introduksjon
                        |> oppdaterSamtaleSteg model
                        |> Success
                    , Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn (ViewportSatt >> SuccessMsg)
                        , case MeldingsLogg.ferdigAnimert nyMeldingslogg of
                            FerdigAnimert _ ->
                                Cmd.none

                            MeldingerGjenstår ->
                                200
                                    |> DebugStatus.meldingsTimeout model.debugStatus
                                    |> Process.sleep
                                    |> Task.perform (always (SuccessMsg StartÅSkrive))
                        ]
                    )

                _ ->
                    ( Success model, Cmd.none )

        SpråkMsg msg ->
            case model.aktivSamtale of
                SpråkSeksjon språkModel ->
                    case Sprak.Seksjon.update msg språkModel of
                        Sprak.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SpråkSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (SpråkMsg >> SuccessMsg) cmd
                            )

                        Sprak.Seksjon.Ferdig meldingsLogg ->
                            gåTilSeksjonsValg model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        FørerkortMsg msg ->
            case model.aktivSamtale of
                FørerkortSeksjon førerkortModel ->
                    case Forerkort.Seksjon.update msg førerkortModel of
                        Forerkort.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> FørerkortSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (FørerkortMsg >> SuccessMsg) cmd
                            )

                        Forerkort.Seksjon.Ferdig meldingsLogg ->
                            gåTilSeksjonsValg model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        SammendragMsg msg ->
            case model.aktivSamtale of
                SammendragSeksjon sammendragModel ->
                    case Seksjon.Sammendrag.update msg sammendragModel of
                        Seksjon.Sammendrag.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SammendragSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (SammendragMsg >> SuccessMsg) cmd
                            )

                        Seksjon.Sammendrag.Ferdig meldingsLogg ->
                            gåTilAvslutning model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        AvslutningMsg msg ->
            case model.aktivSamtale of
                AvslutningSeksjon avslutningModel ->
                    case Seksjon.Avslutning.update msg avslutningModel of
                        Seksjon.Avslutning.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> AvslutningSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (AvslutningMsg >> SuccessMsg) cmd
                            )

                        Seksjon.Avslutning.Ferdig nyModel meldingsLogg ->
                            ( Success model, Cmd.none )

                _ ->
                    ( Success model, Cmd.none )

        SeksjonsvalgMsg msg ->
            case model.aktivSamtale of
                SeksjonsvalgSeksjon seksjonsvalgModel ->
                    case Seksjon.Seksjonsvalg.update msg seksjonsvalgModel of
                        Seksjon.Seksjonsvalg.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SeksjonsvalgSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (SeksjonsvalgMsg >> SuccessMsg) cmd
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
                                    gåTilFørerkort model meldingsLogg

                                Seksjon.Seksjonsvalg.IngenAvSeksjonene ->
                                    gåTilSammendrag model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        FagdokumentasjonMsg msg ->
            case model.aktivSamtale of
                FagdokumentasjonSeksjon fagdokumentasjonModel ->
                    case Fagdokumentasjon.Seksjon.update msg fagdokumentasjonModel of
                        Fagdokumentasjon.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> FagdokumentasjonSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (FagdokumentasjonMsg >> SuccessMsg) cmd
                            )

                        Fagdokumentasjon.Seksjon.Ferdig fagdokumentasjonListe meldingsLogg ->
                            gåTilFlereSeksjonsValg model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        SertifikatMsg msg ->
            case model.aktivSamtale of
                SertifikatSeksjon sertifikatModel ->
                    case Sertifikat.Seksjon.update msg sertifikatModel of
                        Sertifikat.Seksjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SertifikatSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (SertifikatMsg >> SuccessMsg) cmd
                            )

                        Sertifikat.Seksjon.Ferdig sertifikatListe meldingsLogg ->
                            gåTilFlereSeksjonsValg model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )


oppdaterSamtaleSteg : SuccessModel -> SamtaleSeksjon -> SuccessModel
oppdaterSamtaleSteg model samtaleSeksjon =
    { model
        | aktivSamtale = samtaleSeksjon
    }


gåTilArbeidserfaring : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilArbeidserfaring model ferdigAnimertMeldingsLogg =
    let
        ( arbeidsModell, arbeidsCmd ) =
            Arbeidserfaring.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.arbeidserfaring model.cv)
    in
    ( Success { model | aktivSamtale = ArbeidsErfaringSeksjon arbeidsModell }
    , Cmd.map (ArbeidserfaringsMsg >> SuccessMsg) arbeidsCmd
    )


gåTilUtdanning : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilUtdanning model ferdigAnimertMeldingsLogg =
    let
        ( utdanningModel, utdanningCmd ) =
            Utdanning.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.utdanning model.cv)
    in
    ( Success
        { model
            | aktivSamtale = UtdanningSeksjon utdanningModel
        }
    , Cmd.map (UtdanningsMsg >> SuccessMsg) utdanningCmd
    )


gåTilSpråk : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilSpråk model ferdigAnimertMeldingsLogg =
    let
        ( språkModel, språkCmd ) =
            Sprak.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.spraakferdighet model.cv)
    in
    ( Success
        { model
            | aktivSamtale = SpråkSeksjon språkModel
        }
    , Cmd.map (SpråkMsg >> SuccessMsg) språkCmd
    )


gåTilFørerkort : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilFørerkort model ferdigAnimertMeldingsLogg =
    let
        ( førerkortModel, førerkortCmd ) =
            Forerkort.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.forerkort model.cv)
    in
    ( Success
        { model
            | aktivSamtale = FørerkortSeksjon førerkortModel
        }
    , Cmd.map (FørerkortMsg >> SuccessMsg) førerkortCmd
    )


gåTilFagbrev : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilFagbrev model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initFagbrev model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( Success
        { model
            | aktivSamtale = FagdokumentasjonSeksjon fagbrevModel
        }
    , Cmd.map (FagdokumentasjonMsg >> SuccessMsg) fagbrevCmd
    )


gåTilMesterbrev : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilMesterbrev model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initMesterbrev model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( Success
        { model
            | aktivSamtale = FagdokumentasjonSeksjon fagbrevModel
        }
    , Cmd.map (FagdokumentasjonMsg >> SuccessMsg) fagbrevCmd
    )


gåTilAutorisasjon : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilAutorisasjon model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Fagdokumentasjon.Seksjon.initAutorisasjon model.debugStatus ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( Success
        { model
            | aktivSamtale = FagdokumentasjonSeksjon fagbrevModel
        }
    , Cmd.map (FagdokumentasjonMsg >> SuccessMsg) fagbrevCmd
    )


gåTilSertifisering : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilSertifisering model ferdigAnimertMeldingsLogg =
    let
        ( sertifikatModel, sertifikatCmd ) =
            Sertifikat.Seksjon.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.sertifikater model.cv)
    in
    ( Success
        { model
            | aktivSamtale = SertifikatSeksjon sertifikatModel
        }
    , Cmd.map (SertifikatMsg >> SuccessMsg) sertifikatCmd
    )


gåTilSammendrag : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilSammendrag model ferdigAnimertMeldingsLogg =
    let
        ( sammendragModel, sammendragCmd ) =
            Seksjon.Sammendrag.init model.debugStatus ferdigAnimertMeldingsLogg (Cv.sammendrag model.cv)
    in
    ( Success
        { model
            | aktivSamtale = SammendragSeksjon sammendragModel
        }
    , Cmd.map (SammendragMsg >> SuccessMsg) sammendragCmd
    )


gåTilAvslutning : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilAvslutning model ferdigAnimertMeldingsLogg =
    let
        ( avslutningModel, avslutningCmd ) =
            Seksjon.Avslutning.init model.debugStatus model.personalia model.cv ferdigAnimertMeldingsLogg
    in
    ( Success
        { model
            | aktivSamtale = AvslutningSeksjon avslutningModel
        }
    , Cmd.map (AvslutningMsg >> SuccessMsg) avslutningCmd
    )


gåTilSeksjonsValg : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilSeksjonsValg model ferdigAnimertMeldingsLogg =
    let
        ( seksjonsvalgModel, seksjonsvalgCmd ) =
            Seksjon.Seksjonsvalg.initLeggTil model.debugStatus ferdigAnimertMeldingsLogg
    in
    ( Success
        { model
            | aktivSamtale = SeksjonsvalgSeksjon seksjonsvalgModel
        }
    , Cmd.map (SeksjonsvalgMsg >> SuccessMsg) seksjonsvalgCmd
    )


gåTilFlereSeksjonsValg : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilFlereSeksjonsValg model ferdigAnimertMeldingsLogg =
    let
        ( seksjonsvalgModel, seksjonsvalgCmd ) =
            Seksjon.Seksjonsvalg.initLeggTilFlere model.debugStatus ferdigAnimertMeldingsLogg
    in
    ( Success
        { model
            | aktivSamtale = SeksjonsvalgSeksjon seksjonsvalgModel
        }
    , Cmd.map (SeksjonsvalgMsg >> SuccessMsg) seksjonsvalgCmd
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
    case successModel.aktivSamtale of
        Introduksjon logg ->
            logg

        PersonaliaSeksjon model ->
            Personalia.Seksjon.meldingsLogg model

        UtdanningSeksjon model ->
            Utdanning.Seksjon.meldingsLogg model

        ArbeidsErfaringSeksjon model ->
            Arbeidserfaring.Seksjon.meldingsLogg model

        SpråkSeksjon model ->
            Sprak.Seksjon.meldingsLogg model

        FørerkortSeksjon model ->
            Forerkort.Seksjon.meldingsLogg model

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
            , viewBrukerInput successModel.aktivSamtale
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
        Introduksjon logg ->
            case MeldingsLogg.ferdigAnimert logg of
                FerdigAnimert _ ->
                    Containers.knapper Flytende
                        [ Knapp.knapp (SuccessMsg BrukerSierHeiIIntroduksjonen) "Ja!"
                            |> Knapp.toHtml
                        ]

                MeldingerGjenstår ->
                    text ""

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

        FørerkortSeksjon førerkortSeksjon ->
            førerkortSeksjon
                |> Forerkort.Seksjon.viewBrukerInput
                |> Html.map (FørerkortMsg >> SuccessMsg)

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
            case successModel.aktivSamtale of
                Introduksjon _ ->
                    Sub.none

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

                FørerkortSeksjon _ ->
                    Sub.none

                FagdokumentasjonSeksjon _ ->
                    Sub.none

                SertifikatSeksjon _ ->
                    Sub.none

                SeksjonsvalgSeksjon _ ->
                    Sub.none

                AvslutningSeksjon _ ->
                    Sub.none

                SammendragSeksjon _ ->
                    Sub.none
