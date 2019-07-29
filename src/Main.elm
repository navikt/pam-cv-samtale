module Main exposing (main)

import Api
import Browser
import Browser.Dom as Dom
import Browser.Events
import Cv.Cv as Cv exposing (Cv)
import Cv.Utdanning as Utdanning exposing (Utdanning)
import Feilmelding
import FrontendModuler.Header as Header
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Spinner as Spinner
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabel)
import Html.Events exposing (..)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Personalia exposing (Personalia)
import Process
import SamtaleAnimasjon
import Seksjon.Arbeidserfaring
import Seksjon.Personalia
import Seksjon.Utdanning
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, fill, viewBox)
import Task



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
    , windowWidth : Int
    }


type SamtaleSeksjon
    = Introduksjon MeldingsLogg
    | PersonaliaSeksjon Seksjon.Personalia.Model
    | UtdanningSeksjon Seksjon.Utdanning.Model
    | ArbeidsErfaringSeksjon Seksjon.Arbeidserfaring.Model



--- UPDATE ---


type Msg
    = LoadingMsg LoadingMsg
    | SuccessMsg SuccessMsg
    | ErrorLogget (Result Http.Error ())
    | WindowResized Int Int


type LoadingMsg
    = PersonHentet (Result Http.Error ())
    | PersonOpprettet (Result Http.Error ())
    | PersonaliaHentet (Result Http.Error Personalia)
    | PersonaliaOpprettet (Result Http.Error Personalia)
    | CvHentet (Result Http.Error Cv)
    | CvOpprettet (Result Http.Error Cv)
    | RegistreringsProgresjonHentet (Result Http.Error RegistreringsProgresjon)
    | ViewportHentet Dom.Viewport


type SuccessMsg
    = BrukerSierHeiIIntroduksjonen
    | ViewportSatt (Result Dom.Error ())
    | PersonaliaMsg Seksjon.Personalia.Msg
    | UtdanningsMsg Seksjon.Utdanning.Msg
    | StartÅSkrive
    | FullførMelding
    | ArbeidserfaringsMsg Seksjon.Arbeidserfaring.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingMsg lm ->
            updateLoading lm model

        SuccessMsg successMsg ->
            case model of
                Success successModel ->
                    updateSuccess successMsg successModel

                _ ->
                    ( model, Cmd.none )

        ErrorLogget _ ->
            ( model, Cmd.none )

        WindowResized windowWidth _ ->
            updateWindowResized model windowWidth


updateWindowResized : Model -> Int -> ( Model, Cmd msg )
updateWindowResized model windowWidth =
    case model of
        Success successModel ->
            ( Success { successModel | windowWidth = windowWidth }
            , Cmd.none
            )

        Loading (VenterPåResten loadingState) ->
            ( Loading (VenterPåResten { loadingState | windowWidth = Just windowWidth })
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



--- Loading ---


updateLoading : LoadingMsg -> Model -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        PersonHentet result ->
            case result of
                Ok _ ->
                    ( Loading VenterPåPersonalia, Api.hentPersonalia (PersonaliaHentet >> LoadingMsg) )

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            ( model, Api.opprettPerson (PersonOpprettet >> LoadingMsg) )

                        _ ->
                            ( Failure error
                            , logFeilmelding error "Hent Person"
                            )

        PersonOpprettet result ->
            case result of
                Ok _ ->
                    ( Loading VenterPåPersonalia, Api.hentPersonalia (PersonaliaHentet >> LoadingMsg) )

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
                            ( model, Api.opprettPersonalia (PersonaliaOpprettet >> LoadingMsg) )

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
                                    ( model, Api.opprettCv (CvOpprettet >> LoadingMsg) )

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

        ViewportHentet viewport ->
            case model of
                Loading (VenterPåResten state) ->
                    modelFraLoadingState
                        { state
                            | windowWidth =
                                viewport.scene.width
                                    |> round
                                    |> Just
                        }

                _ ->
                    ( model, Cmd.none )


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


modelFraLoadingState : LoadingState -> ( Model, Cmd Msg )
modelFraLoadingState state =
    case ( state.cv, state.registreringsProgresjon, state.windowWidth ) of
        ( Just cv, Just registreringsProgresjon, Just windowWidth ) ->
            ( Success
                { cv = cv
                , personalia = state.personalia
                , registreringsProgresjon = registreringsProgresjon
                , aktivSamtale = initialiserSamtale
                , windowWidth = windowWidth
                }
            , Process.sleep 200
                |> Task.perform (\_ -> SuccessMsg StartÅSkrive)
            )

        _ ->
            ( Loading (VenterPåResten state), Cmd.none )


initialiserSamtale : SamtaleSeksjon
initialiserSamtale =
    MeldingsLogg.init
        |> MeldingsLogg.leggTilSpørsmål
            [ Melding.spørsmål [ "Hei" ]
            , Melding.spørsmål
                [ "Velkommen til CV-registrering!"
                , "Det vi skal gjennom nå er utdanning, arbeidserfaring, språk og sammendrag."
                , "Etter det kan du velge å legge til blant annet kurs, sertifisering, fagbrev, sertifikat og førerkort."
                , "Er du klar til å begynne?"
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
    , Cmd.batch
        [ Api.hentCv (CvHentet >> LoadingMsg)
        , Dom.getViewport
            |> Task.perform (ViewportHentet >> LoadingMsg)
        ]
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
                            personaliaTilArbeidserfaring model personaliaMeldingsLogg

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
                            ( Success model, Cmd.none )

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
                            ( Success model, Cmd.map (ArbeidserfaringsMsg >> SuccessMsg) Seksjon.Arbeidserfaring.lagtTilSpørsmålCmd )

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
                        , Process.sleep 1000
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


oppdaterSamtaleSteg : SuccessModel -> SamtaleSeksjon -> SuccessModel
oppdaterSamtaleSteg model samtaleSeksjon =
    { model
        | aktivSamtale = samtaleSeksjon
    }


personaliaFerdig : SuccessModel -> Personalia -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
personaliaFerdig model personalia ferdigAnimertMeldingsLogg =
    ( Success
        { model
            | aktivSamtale = UtdanningSeksjon (Seksjon.Utdanning.init (MeldingsLogg.tilMeldingsLogg ferdigAnimertMeldingsLogg) (Cv.utdanning model.cv))
        }
    , Cmd.none
    )


personaliaTilArbeidserfaring : SuccessModel -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
personaliaTilArbeidserfaring model ferdigAnimertMeldingsLogg =
    let
        ( arbeidsModell, arbeidsCmd ) =
            Seksjon.Arbeidserfaring.init ferdigAnimertMeldingsLogg
    in
    ( Success { model | aktivSamtale = ArbeidsErfaringSeksjon arbeidsModell }
    , Cmd.map (ArbeidserfaringsMsg >> SuccessMsg) arbeidsCmd
    )



{--
utdanningFerdig : SuccessModel -> List Utdanning -> MeldingsLogg -> ( Model, Cmd Msg )
utdanningFerdig model utdanning utdanningMeldingsLogg =
    ( Success
        { model
            | aktivSamtale = ArbeidsErfaringSeksjon (Seksjon.Arbeidserfaring.init utdanningMeldingsLogg)
        }
    , Cmd.map (ArbeidserfaringsMsg >> SuccessMsg) Seksjon.Arbeidserfaring.lagtTilSpørsmålCmd
    )
--}
--- VIEW ---


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            viewLoading

        Success successModel ->
            viewSuccess successModel

        Failure error ->
            text "error"


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


viewSuccess : SuccessModel -> Html Msg
viewSuccess successModel =
    div [ class "app" ]
        [ Header.header successModel.windowWidth
            |> Header.toHtml
        , div [ class "samtale-wrapper", id "samtale" ]
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
        ]


viewMeldingsLogg : MeldingsLogg -> Html Msg
viewMeldingsLogg meldingsLogg =
    meldingsLogg
        |> MeldingsLogg.meldinger
        |> List.map viewMelding
        |> div []


viewMelding : Melding -> Html Msg
viewMelding melding =
    div [ class ("meldingsrad " ++ meldingsClass melding) ]
        [ div [ class "melding" ]
            [ Melding.innhold melding
                |> List.map (\elem -> p [] [ text elem ])
                |> div []
            ]
        ]


meldingsClass : Melding -> String
meldingsClass melding =
    case Melding.meldingsType melding of
        Melding.Spørsmål ->
            "sporsmal"

        Melding.Svar ->
            "svar"


viewSkriveStatus : MeldingsLogg -> Html msg
viewSkriveStatus meldingsLogg =
    case MeldingsLogg.skriveStatus meldingsLogg of
        MeldingsLogg.Skriver ->
            div [ class "meldingsrad sporsmal" ]
                [ div [ class "melding" ]
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
                    div [ class "inputrad" ]
                        [ Knapp.knapp (SuccessMsg BrukerSierHeiIIntroduksjonen) "Ja!"
                            |> Knapp.toHtml
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

        ArbeidsErfaringSeksjon arbeidserfaringSeksjon ->
            arbeidserfaringSeksjon
                |> Seksjon.Arbeidserfaring.viewBrukerInput
                |> Html.map (ArbeidserfaringsMsg >> SuccessMsg)



--- PROGRAM ---


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always (Browser.Events.onResize WindowResized)
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Loading VenterPåPerson, Api.hentPerson (PersonHentet >> LoadingMsg) )
