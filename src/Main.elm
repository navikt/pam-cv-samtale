module Main exposing (main)

import Api
import Browser
import Html exposing (..)
import Http
import Personalia exposing (Personalia)



--- MODEL ---


type alias Cv =
    ()


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



--- UPDATE ---


type Msg
    = PersonaliaHentet (Result Http.Error Personalia)
    | PersonaliaOpprettet (Result Http.Error Personalia)
    | CvHentet (Result Http.Error Cv)
    | RegistreringsProgresjonHentet (Result Http.Error RegistreringsProgresjon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersonaliaHentet result ->
            case model of
                Loading state ->
                    case result of
                        Ok personalia ->
                            ( Loading { state | personalia = Just personalia }, Cmd.none )

                        Err error ->
                            case error of
                                Http.BadStatus 404 ->
                                    ( model, Api.opprettPersonalia PersonaliaOpprettet )

                                _ ->
                                    ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PersonaliaOpprettet result ->
            case model of
                Loading state ->
                    case result of
                        Ok personalia ->
                            ( Loading { state | personalia = Just personalia }, Cmd.none )

                        Err error ->
                            ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CvHentet result ->
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
            text "Error"


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


init : () -> ( Model, Cmd Msg )
init flags =
    ( Loading
        { cv = Nothing
        , personalia = Nothing
        , registreringsProgresjon = Nothing
        }
    , Api.hentPersonalia PersonaliaHentet
    )
