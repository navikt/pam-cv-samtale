module Main exposing (SuccessMsg, main)

import Api
import Browser
import Cv.Cv as Cv exposing (Cv)
import Feilmelding
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Personalia exposing (Personalia)
import Seksjon.Arbeidserfaring
import Seksjon.Personalia
import Snakkeboble exposing (Snakkeboble(..))



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
    }


type alias SuccessModel =
    { cv : Cv
    , personalia : Personalia
    , registreringsProgresjon : RegistreringsProgresjon
    , aktivSamtale : SamtaleSeksjon
    , historikk : List Snakkeboble
    }


type SamtaleSeksjon
    = Introduksjon
    | PersonaliaSeksjon Seksjon.Personalia.Model
    | Utdanning
    | ArbeidsErfaring



--- UPDATE ---


type Msg
    = LoadingMsg LoadingMsg
    | SuccessMsg SuccessMsg
    | ErrorLogget (Result Http.Error ())


type LoadingMsg
    = PersonHentet (Result Http.Error ())
    | PersonOpprettet (Result Http.Error ())
    | PersonaliaHentet (Result Http.Error Personalia)
    | PersonaliaOpprettet (Result Http.Error Personalia)
    | CvHentet (Result Http.Error Cv)
    | CvOpprettet (Result Http.Error Cv)
    | RegistreringsProgresjonHentet (Result Http.Error RegistreringsProgresjon)


type SuccessMsg
    = BrukerSierHeiIIntroduksjonen
    | PersonaliaMsg Seksjon.Personalia.Msg
    | ArbeidserfaringMsg Seksjon.Arbeidserfaring.Msg


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
                            ( modelFraLoadingState { state | cv = Just cv }, Cmd.none )

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
                            ( modelFraLoadingState { state | cv = Just cv }, Cmd.none )

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


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error message =
    Feilmelding.feilmelding "message" error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


modelFraLoadingState : LoadingState -> Model
modelFraLoadingState state =
    case ( state.cv, state.registreringsProgresjon ) of
        ( Just cv, Just registreringsProgresjon ) ->
            Success
                { cv = cv
                , personalia = state.personalia
                , registreringsProgresjon = registreringsProgresjon
                , aktivSamtale = Introduksjon
                , historikk = []
                }

        _ ->
            Loading (VenterPåResten state)


initVenterPåResten : Personalia -> ( Model, Cmd Msg )
initVenterPåResten personalia =
    ( Loading
        (VenterPåResten
            { cv = Nothing
            , personalia = personalia
            , registreringsProgresjon =
                Just
                    { erDetteFørsteGangManErInneILøsningen = True
                    , personalia = IkkeBegynt
                    , utdanning = IkkeBegynt
                    }
            }
        )
    , Api.hentCv (CvHentet >> LoadingMsg)
    )



--- Success ---


updateSuccess : SuccessMsg -> SuccessModel -> ( Model, Cmd Msg )
updateSuccess successMsg model =
    case successMsg of
        BrukerSierHeiIIntroduksjonen ->
            ( nesteSamtaleSteg model "Hei!" (PersonaliaSeksjon (Seksjon.Personalia.init model.personalia)) |> Success, Cmd.none )

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

                        Seksjon.Personalia.Ferdig personalia historikk ->
                            personaliaFerdig model personalia historikk

                _ ->
                    ( Success model, Cmd.none )

        ArbeidserfaringMsg msg ->
            ( Success model, Cmd.none )


nesteSamtaleSteg : SuccessModel -> String -> SamtaleSeksjon -> SuccessModel
nesteSamtaleSteg model tekst samtaleSeksjon =
    { model
        | aktivSamtale = samtaleSeksjon
        , historikk = model.historikk ++ [ samtaleTilBoble model.aktivSamtale, Bruker tekst ]
    }


personaliaFerdig : SuccessModel -> Personalia -> List Snakkeboble -> ( Model, Cmd Msg )
personaliaFerdig model personalia historikk =
    ( Success
        { model
            | historikk = model.historikk ++ historikk
            , aktivSamtale = Utdanning
        }
    , Cmd.none
    )


oppdaterSamtaleSteg : SuccessModel -> SamtaleSeksjon -> SuccessModel
oppdaterSamtaleSteg model samtaleSeksjon =
    { model
        | aktivSamtale = samtaleSeksjon
    }



--- VIEW ---


view : Model -> Html Msg
view model =
    case model of
        Loading loadingState ->
            text "spinner"

        Success successModel ->
            viewSuccess successModel

        Failure error ->
            text "error"


viewSuccess : SuccessModel -> Html Msg
viewSuccess successModel =
    div []
        [ viewHistorikk (successModel.historikk ++ seksjonshistorikk successModel)
        , viewAktivSamtale successModel.aktivSamtale
        ]


seksjonshistorikk : SuccessModel -> List Snakkeboble
seksjonshistorikk successModel =
    case successModel.aktivSamtale of
        Introduksjon ->
            []

        PersonaliaSeksjon model ->
            Seksjon.Personalia.historikk model

        Utdanning ->
            []

        ArbeidsErfaring ->
            []


viewHistorikk : List Snakkeboble -> Html Msg
viewHistorikk historikk =
    div [] <| List.map viewSnakkeboble historikk


viewSnakkeboble : Snakkeboble -> Html Msg
viewSnakkeboble snakkeboble =
    case snakkeboble of
        Robot string ->
            div []
                [ text ("Robot: " ++ string)
                ]

        Bruker string ->
            div []
                [ text ("Bruker: " ++ string)
                ]


samtaleTilBoble : SamtaleSeksjon -> Snakkeboble
samtaleTilBoble samtalesteg =
    case samtalesteg of
        Introduksjon ->
            Robot "Hei!"

        PersonaliaSeksjon personaliaSeksjon ->
            Robot "Ikke implementert"

        _ ->
            Robot "Ikke implementert"


viewAktivSamtale : SamtaleSeksjon -> Html Msg
viewAktivSamtale aktivSamtale =
    div []
        [ aktivSamtale
            |> samtaleTilBoble
            |> viewSnakkeboble
        , viewBrukerInput aktivSamtale
        ]


viewBrukerInput : SamtaleSeksjon -> Html Msg
viewBrukerInput aktivSamtale =
    case aktivSamtale of
        Introduksjon ->
            button [ onClick (SuccessMsg BrukerSierHeiIIntroduksjonen) ] [ text "Hei!" ]

        PersonaliaSeksjon personaliaSeksjon ->
            personaliaSeksjon
                |> Seksjon.Personalia.viewBrukerInput
                |> Html.map (PersonaliaMsg >> SuccessMsg)

        _ ->
            text ""



--- PROGRAM ---


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Loading VenterPåPerson, Api.hentPerson (PersonHentet >> LoadingMsg) )
