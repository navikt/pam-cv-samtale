module Main exposing (main)

import Api
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Navigation
import Cv.Cv as Cv exposing (Cv)
import Feilmelding
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
import Process
import SamtaleAnimasjon
import Seksjon.Arbeidserfaring
import Seksjon.Avslutning
import Seksjon.Fagdokumentasjon
import Seksjon.Personalia
import Seksjon.Sammendrag
import Seksjon.Seksjonsvalg
import Seksjon.Sprak
import Seksjon.Utdanning
import Task
import Url



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
    }


type SamtaleSeksjon
    = Introduksjon MeldingsLogg
    | PersonaliaSeksjon Seksjon.Personalia.Model
    | UtdanningSeksjon Seksjon.Utdanning.Model
    | ArbeidsErfaringSeksjon Seksjon.Arbeidserfaring.Model
    | SpråkSeksjon Seksjon.Sprak.Model
    | FagdokumentasjonSeksjon Seksjon.Fagdokumentasjon.Model
    | SeksjonsvalgSeksjon Seksjon.Seksjonsvalg.Model
    | AvslutningSeksjon Seksjon.Avslutning.Model
    | SammendragSeksjon Seksjon.Sammendrag.Model



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
    | PersonaliaMsg Seksjon.Personalia.Msg
    | UtdanningsMsg Seksjon.Utdanning.Msg
    | ArbeidserfaringsMsg Seksjon.Arbeidserfaring.Msg
    | SpråkMsg Seksjon.Sprak.Msg
    | SeksjonsvalgMsg Seksjon.Seksjonsvalg.Msg
    | SammendragMsg Seksjon.Sammendrag.Msg
    | FagdokumentasjonMsg Seksjon.Fagdokumentasjon.Msg
    | AvslutningMsg Seksjon.Avslutning.Msg
    | StartÅSkrive
    | FullførMelding


update : Msg -> ExtendedModel -> ( ExtendedModel, Cmd Msg )
update msg extendedModel =
    case msg of
        LoadingMsg loadingModel ->
            updateLoading extendedModel.navigationKey loadingModel extendedModel.model
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
              , windowWidth = round viewport.scene.width
              }
            , Cmd.none
            )


mapTilExtendedModel : ExtendedModel -> ( Model, Cmd Msg ) -> ( ExtendedModel, Cmd Msg )
mapTilExtendedModel extendedModel ( model, cmd ) =
    ( { model = model
      , windowWidth = extendedModel.windowWidth
      , navigationKey = extendedModel.navigationKey
      }
    , cmd
    )



--- Loading ---


updateLoading : Navigation.Key -> LoadingMsg -> Model -> ( Model, Cmd Msg )
updateLoading navigationKey msg model =
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
                            modelFraLoadingState { state | cv = Just cv }

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
                            modelFraLoadingState { state | cv = Just cv }

                        Err error ->
                            ( Failure error
                            , logFeilmelding error "Opprett CV"
                            )

                _ ->
                    ( model, Cmd.none )

        RegistreringsProgresjonHentet result ->
            case result of
                Ok value ->
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


modelFraLoadingState : LoadingState -> ( Model, Cmd Msg )
modelFraLoadingState state =
    case ( state.cv, state.registreringsProgresjon ) of
        ( Just cv, Just registreringsProgresjon ) ->
            ( Success
                { cv = cv
                , personalia = state.personalia
                , registreringsProgresjon = registreringsProgresjon
                , aktivSamtale = initialiserSamtale state.personalia
                }
            , Process.sleep 200
                |> Task.perform (\_ -> SuccessMsg StartÅSkrive)
            )

        _ ->
            ( Loading (VenterPåResten state), Cmd.none )


initialiserSamtale : Personalia -> SamtaleSeksjon
initialiserSamtale personalia =
    MeldingsLogg.init
        |> MeldingsLogg.leggTilSpørsmål
            [ Melding.spørsmål [ "Hei " ++ (Personalia.fornavn personalia |> Maybe.withDefault "") ++ "! Jeg er roboten CVert, og jeg kan hjelpe deg med å lage en CV." ]
            , Melding.spørsmål
                [ "Det du skal igjennom nå er utdanning, arbeidserfaring, språk og sammendrag."
                , "Etter det kan du velge å legge til blant annet kurs, sertifisering, fagbrev, sertifisering og førerkort."
                ]
            , Melding.spørsmål
                [ "Er du klar til å begynne?"
                ]
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
                                |> Seksjon.Personalia.init model.personalia
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
                    case Seksjon.Personalia.update msg personaliaModel of
                        Seksjon.Personalia.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> PersonaliaSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (PersonaliaMsg >> SuccessMsg) cmd
                            )

                        Seksjon.Personalia.Ferdig personalia personaliaMeldingsLogg ->
                            gåTilUtdanning model personaliaMeldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        UtdanningsMsg msg ->
            case model.aktivSamtale of
                UtdanningSeksjon utdanningModel ->
                    case Seksjon.Utdanning.update msg utdanningModel of
                        Seksjon.Utdanning.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> UtdanningSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (UtdanningsMsg >> SuccessMsg) cmd
                            )

                        Seksjon.Utdanning.Ferdig utdanning meldingsLogg ->
                            gåTilArbeidserfaring model meldingsLogg

                _ ->
                    ( Success model, Cmd.none )

        ArbeidserfaringsMsg msg ->
            case model.aktivSamtale of
                ArbeidsErfaringSeksjon arbeidserfaringsModel ->
                    case Seksjon.Arbeidserfaring.update msg arbeidserfaringsModel of
                        Seksjon.Arbeidserfaring.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> ArbeidsErfaringSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (ArbeidserfaringsMsg >> SuccessMsg) cmd
                            )

                        Seksjon.Arbeidserfaring.Ferdig ferdigAnimertMeldingsLogg ->
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
                        , Process.sleep (MeldingsLogg.nesteMeldingToString meldingsLogg * 1000.0)
                            --1000
                            |> Task.perform (\_ -> SuccessMsg FullførMelding)
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
                                Process.sleep 200
                                    |> Task.perform (\_ -> SuccessMsg StartÅSkrive)
                        ]
                    )

                _ ->
                    ( Success model, Cmd.none )

        SpråkMsg msg ->
            case model.aktivSamtale of
                SpråkSeksjon språkModel ->
                    case Seksjon.Sprak.update msg språkModel of
                        Seksjon.Sprak.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> SpråkSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (SpråkMsg >> SuccessMsg) cmd
                            )

                        Seksjon.Sprak.Ferdig meldingsLogg ->
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

                        Seksjon.Seksjonsvalg.Ferdig seksjon nyModel meldingsLogg ->
                            -- TODO: her legger man inn caser for hvor seksjonsvalget skal gå
                            case seksjon of
                                "Arbeidserfaring" ->
                                    gåTilArbeidserfaring model meldingsLogg

                                "Utdanning" ->
                                    gåTilUtdanning model meldingsLogg

                                "Språk" ->
                                    gåTilSpråk model meldingsLogg

                                "Nei, gå videre" ->
                                    gåTilSammendrag model meldingsLogg

                                -- FIXME: ikke implementert
                                "Fagbrev/Svennebrev" ->
                                    gåTilFagbrev model meldingsLogg

                                "Mesterbrev" ->
                                    gåTilMesterbrev model meldingsLogg

                                "Autorisasjon" ->
                                    gåTilAutorisasjon model meldingsLogg

                                "Sertifisering" ->
                                    ( Success model, Cmd.none )

                                "Annen erfaring" ->
                                    ( Success model, Cmd.none )

                                "Kurs" ->
                                    ( Success model, Cmd.none )

                                "Førerkort" ->
                                    ( Success model, Cmd.none )

                                _ ->
                                    ( Success model, Cmd.none )

                _ ->
                    ( Success model, Cmd.none )

        FagdokumentasjonMsg msg ->
            case model.aktivSamtale of
                FagdokumentasjonSeksjon fagdokumentasjonModel ->
                    case Seksjon.Fagdokumentasjon.update msg fagdokumentasjonModel of
                        Seksjon.Fagdokumentasjon.IkkeFerdig ( nyModel, cmd ) ->
                            ( nyModel
                                |> FagdokumentasjonSeksjon
                                |> oppdaterSamtaleSteg model
                                |> Success
                            , Cmd.map (FagdokumentasjonMsg >> SuccessMsg) cmd
                            )

                        Seksjon.Fagdokumentasjon.Ferdig fagdokumentasjonListe meldingsLogg ->
                            gåTilSeksjonsValg model meldingsLogg

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
            Seksjon.Arbeidserfaring.init ferdigAnimertMeldingsLogg (Cv.arbeidserfaring model.cv)
    in
    ( Success { model | aktivSamtale = ArbeidsErfaringSeksjon arbeidsModell }
    , Cmd.map (ArbeidserfaringsMsg >> SuccessMsg) arbeidsCmd
    )


gåTilUtdanning : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilUtdanning model ferdigAnimertMeldingsLogg =
    let
        ( utdanningModel, utdanningCmd ) =
            Seksjon.Utdanning.init ferdigAnimertMeldingsLogg (Cv.utdanning model.cv)
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
            Seksjon.Sprak.init ferdigAnimertMeldingsLogg (Cv.spraakferdighet model.cv)
    in
    ( Success
        { model
            | aktivSamtale = SpråkSeksjon språkModel
        }
    , Cmd.map (SpråkMsg >> SuccessMsg) språkCmd
    )


gåTilFagbrev : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilFagbrev model ferdigAnimertMeldingsLogg =
    let
        ( fagbrevModel, fagbrevCmd ) =
            Seksjon.Fagdokumentasjon.initFagbrev ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
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
            Seksjon.Fagdokumentasjon.initMesterbrev ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
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
            Seksjon.Fagdokumentasjon.initAutorisasjon ferdigAnimertMeldingsLogg (Cv.fagdokumentasjoner model.cv)
    in
    ( Success
        { model
            | aktivSamtale = FagdokumentasjonSeksjon fagbrevModel
        }
    , Cmd.map (FagdokumentasjonMsg >> SuccessMsg) fagbrevCmd
    )


gåTilSammendrag : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
gåTilSammendrag model ferdigAnimertMeldingsLogg =
    let
        ( sammendragModel, sammendragCmd ) =
            Seksjon.Sammendrag.init ferdigAnimertMeldingsLogg (Cv.sammendrag model.cv)
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
            Seksjon.Avslutning.init model.personalia model.cv ferdigAnimertMeldingsLogg
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
            Seksjon.Seksjonsvalg.init ferdigAnimertMeldingsLogg
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

                    Http.BadBody string ->
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
            Seksjon.Personalia.meldingsLogg model

        UtdanningSeksjon model ->
            Seksjon.Utdanning.meldingsLogg model

        ArbeidsErfaringSeksjon model ->
            Seksjon.Arbeidserfaring.meldingsLogg model

        SpråkSeksjon model ->
            Seksjon.Sprak.meldingsLogg model

        FagdokumentasjonSeksjon model ->
            Seksjon.Fagdokumentasjon.meldingsLogg model

        SeksjonsvalgSeksjon model ->
            Seksjon.Seksjonsvalg.meldingsLogg model

        SammendragSeksjon model ->
            Seksjon.Sammendrag.meldingsLogg model

        AvslutningSeksjon model ->
            Seksjon.Avslutning.meldingsLogg model


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
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp (SuccessMsg BrukerSierHeiIIntroduksjonen) "Ja!"
                                    |> Knapp.withClass Knapp.MånedKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                MeldingerGjenstår ->
                    text ""

        PersonaliaSeksjon personaliaSeksjon ->
            personaliaSeksjon
                |> Seksjon.Personalia.viewBrukerInput
                |> Html.map (PersonaliaMsg >> SuccessMsg)

        UtdanningSeksjon utdanningSeksjon ->
            utdanningSeksjon
                |> Seksjon.Utdanning.viewBrukerInput
                |> Html.map (UtdanningsMsg >> SuccessMsg)

        SpråkSeksjon språkSeksjon ->
            språkSeksjon
                |> Seksjon.Sprak.viewBrukerInput
                |> Html.map (SpråkMsg >> SuccessMsg)

        SammendragSeksjon sammendragSeksjon ->
            sammendragSeksjon
                |> Seksjon.Sammendrag.viewBrukerInput
                |> Html.map (SammendragMsg >> SuccessMsg)

        ArbeidsErfaringSeksjon arbeidserfaringSeksjon ->
            arbeidserfaringSeksjon
                |> Seksjon.Arbeidserfaring.viewBrukerInput
                |> Html.map (ArbeidserfaringsMsg >> SuccessMsg)

        FagdokumentasjonSeksjon fagbrevSeksjon ->
            fagbrevSeksjon
                |> Seksjon.Fagdokumentasjon.viewBrukerInput
                |> Html.map (FagdokumentasjonMsg >> SuccessMsg)

        AvslutningSeksjon avslutningSeksjon ->
            avslutningSeksjon
                |> Seksjon.Avslutning.viewBrukerInput
                |> Html.map (AvslutningMsg >> SuccessMsg)

        SeksjonsvalgSeksjon seksjonsvalgSeksjon ->
            seksjonsvalgSeksjon
                |> Seksjon.Seksjonsvalg.viewBrukerInput
                |> Html.map (SeksjonsvalgMsg >> SuccessMsg)



--- PROGRAM ---


main =
    Browser.application
        { init = init
        , update = update
        , view = viewDocument
        , subscriptions = always (Browser.Events.onResize WindowResized)
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequestChanged
        }


init : () -> Url.Url -> Navigation.Key -> ( ExtendedModel, Cmd Msg )
init _ _ navigationKey =
    ( { model = Loading VenterPåPerson
      , windowWidth = 1000
      , navigationKey = navigationKey
      }
    , Cmd.batch
        [ Api.getPerson (PersonHentet >> LoadingMsg)
        , Dom.getViewport
            |> Task.perform ViewportHentet
        ]
    )
