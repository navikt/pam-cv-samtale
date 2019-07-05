module Main exposing (main)

import Api
import Browser
import Cv.Cv as Cv exposing (Cv)
import Html exposing (..)
import Http
import Personalia exposing (Personalia)



--- SAMTALE ---


type Samtale
    = Introduksjon
    | PersonaligInformasjon
    | Utdanning
    | ArbeidsErfaring



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
    }


modelFraLoadingState : LoadingState -> Model
modelFraLoadingState state =
    case ( state.cv, state.registreringsProgresjon ) of
        ( Just cv, Just registreringsProgresjon ) ->
            Success
                { cv = cv
                , personalia = state.personalia
                , registreringsProgresjon = registreringsProgresjon
                }

        _ ->
            Loading (VenterPåResten state)



--- UPDATE ---


type Msg
    = LoadingMsg LoadingMsg
    | SuccessMsg SuccessMsg


type SuccessMsg
    = KlarTilÅStarte
    | OppdaterPersonalia


type LoadingMsg
    = PersonHentet (Result Http.Error ())
    | PersonOpprettet (Result Http.Error ())
    | PersonaliaHentet (Result Http.Error Personalia)
    | PersonaliaOpprettet (Result Http.Error Personalia)
    | CvHentet (Result Http.Error Cv)
    | CvOpprettet (Result Http.Error Cv)
    | RegistreringsProgresjonHentet (Result Http.Error RegistreringsProgresjon)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadingMsg lm ->
            updateLoading lm model

        SuccessMsg _ ->
            case model of
                Success successModel ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


updateSuccessModel : SuccessMsg -> SuccessModel -> ( Model, Cmd Msg )
updateSuccessModel successMsg model =
    case successMsg of
        KlarTilÅStarte ->
            ( Success model, Cmd.none )

        OppdaterPersonalia ->
            ( Success model, Cmd.none )


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
                            ( Failure error, Cmd.none )

        PersonOpprettet result ->
            case result of
                Ok _ ->
                    ( Loading VenterPåPersonalia, Api.hentPersonalia (PersonaliaHentet >> LoadingMsg) )

                Err error ->
                    ( Failure error, Cmd.none )

        PersonaliaHentet result ->
            case result of
                Ok personalia ->
                    initVenterPåResten personalia

                Err error ->
                    case error of
                        Http.BadStatus 404 ->
                            ( model, Api.opprettPersonalia (PersonaliaOpprettet >> LoadingMsg) )

                        _ ->
                            ( Failure error, Cmd.none )

        PersonaliaOpprettet result ->
            case result of
                Ok personalia ->
                    initVenterPåResten personalia

                Err error ->
                    ( Failure error, Cmd.none )

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
                                    ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CvOpprettet result ->
            case model of
                Loading (VenterPåResten state) ->
                    case result of
                        Ok cv ->
                            ( modelFraLoadingState { state | cv = Just cv }, Cmd.none )

                        Err error ->
                            ( Failure error, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RegistreringsProgresjonHentet result ->
            ( model, Cmd.none )


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
    div []
        [ div []
            [ text "Navn: "
            , text (Maybe.withDefault "Kunne ikke finne fornavn" (Personalia.fornavn successModel.personalia))
            , text (Maybe.withDefault "Kunne ikke finne fornavn" (Personalia.etternavn successModel.personalia))
            ]
        , div []
            [ text "Telefonnumer: "
            , text (Maybe.withDefault "Kunne ikke finne fornavn" (Personalia.telefon successModel.personalia))
            ]
        ]



{--
    text
        (Maybe.withDefault "Kunne ikke finne fornavn" (Personalia.fornavn successModel.personalia)
            ++ "\n"
            ++ Maybe.withDefault "Kunne ikke finne etternavn" (Personalia.etternavn successModel.personalia)
            ++ "\n"
            ++ Maybe.withDefault "Kunne ikke finne telefonnummer" (Personalia.telefon successModel.personalia)
        )
--}
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
