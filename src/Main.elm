module Main exposing (main)

import Api
import Browser
import Cv.Cv as Cv exposing (Cv)
import Html exposing (..)
import Http
import Personalia exposing (Personalia)



--- MODEL ---


type alias RegistreringsProgresjon =
    { erDetteFørsteGangManErInneILøsningen : Bool
    , personalia : Seksjonsstatus
    , utdanning : Seksjonsstatus
    }


type Seksjonsstatus
    = IkkeBegynt
    | Begynt
    | Fullført


type Model
    = Loading LoadingState
    | Success SuccessModel
    | Failure Http.Error


type alias LoadingState =
    { cv : Maybe Cv
    , personalia : Maybe Personalia
    , registreringsProgresjon : Maybe RegistreringsProgresjon
    }


type alias SuccessModel =
    { cv : Cv
    , personalia : Personalia
    , registreringsProgresjon : RegistreringsProgresjon
    }


modelFraLoadingState : LoadingState -> Model
modelFraLoadingState state =
    case ( state.cv, state.personalia, state.registreringsProgresjon ) of
        ( Just cv, Just personalia, Just registreringsProgresjon ) ->
            Success
                { cv = cv
                , personalia = personalia
                , registreringsProgresjon = registreringsProgresjon
                }

        _ ->
            Loading state



--- UPDATE ---


type Msg
    = LoadingMsg LoadingMsg
    | SuccessMsg


type LoadingMsg
    = PersonaliaHentet (Result Http.Error Personalia)
    | PersonaliaOpprettet (Result Http.Error Personalia)
    | CvHentet (Result Http.Error Cv)
    | CvOpprettet (Result Http.Error Cv)
    | RegistreringsProgresjonHentet (Result Http.Error RegistreringsProgresjon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingMsg lm ->
            updateLoading lm model

        SuccessMsg ->
            ( model, Cmd.none )



{--
TODO: FIXME
Msg =/= LoadingMsg
--}


updateLoading : LoadingMsg -> Model -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        PersonaliaHentet result ->
            case model of
                Loading state ->
                    case result of
                        Ok personalia ->
                            ( modelFraLoadingState { state | personalia = Just personalia }, Cmd.none )

                        Err error ->
                            case error of
                                Http.BadStatus 404 ->
                                    ( model, Api.opprettPersonalia (PersonaliaOpprettet >> LoadingMsg) )

                                _ ->
                                    ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PersonaliaOpprettet result ->
            case model of
                Loading state ->
                    case result of
                        Ok personalia ->
                            ( modelFraLoadingState { state | personalia = Just personalia }, Cmd.none )

                        Err error ->
                            ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CvHentet result ->
            case model of
                Loading state ->
                    case result of
                        Ok cv ->
                            ( modelFraLoadingState { state | cv = Just cv }, Cmd.none )

                        Err error ->
                            case error of
                                Http.BadStatus 404 ->
                                    ( model, Api.opprettCv (CvOpprettet >> LoadingMsg) )

                                _ ->
                                    ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CvOpprettet result ->
            case model of
                Loading state ->
                    case result of
                        Ok cv ->
                            ( modelFraLoadingState { state | cv = Just cv }, Cmd.none )

                        Err error ->
                            ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RegistreringsProgresjonHentet result ->
            ( model, Cmd.none )



--- VIEW ---


view : Model -> Html Msg
view model =
    case model of
        Loading loadingState ->
            text "spinner"

        Success successModel ->
            viewSuccess successModel

        Failure error ->
            Debug.toString error |> text


viewSuccess : SuccessModel -> Html Msg
viewSuccess successModel =
    text "Success"



--- PROGRAM ---


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd LoadingMsg )
init flags =
    ( Loading
        { cv = Nothing
        , personalia = Nothing
        , registreringsProgresjon =
            Just
                { erDetteFørsteGangManErInneILøsningen = True
                , personalia = IkkeBegynt
                , utdanning = IkkeBegynt
                }
        }
    , Cmd.batch
        [ Api.hentPersonalia PersonaliaHentet
        , Api.hentCv CvHentet
        ]
    )
