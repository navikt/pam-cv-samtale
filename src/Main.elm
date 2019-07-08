module Main exposing (main)

import Api
import Browser
import Cv.Cv as Cv exposing (Cv)
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Personalia exposing (Personalia)
import Skjema.Personalia exposing (PersonaliaSkjema)



--- SAMTALE ---


type SamtaleSeksjon
    = Introduksjon
    | PersonaliaSeksjon PersonaliaSeksjon
    | Utdanning
    | ArbeidsErfaring


type PersonaliaSeksjon
    = BekreftOriginal Personalia
    | EndreOriginal PersonaliaSkjema


type alias Samtale =
    { aktivSamtale : SamtaleSeksjon
    , historikk : List Snakkeboble
    }


type Snakkeboble
    = Robot String
    | Bruker String



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
    , samtale : Samtale
    }


modelFraLoadingState : LoadingState -> Model
modelFraLoadingState state =
    case ( state.cv, state.registreringsProgresjon ) of
        ( Just cv, Just registreringsProgresjon ) ->
            Success
                { cv = cv
                , personalia = state.personalia
                , registreringsProgresjon = registreringsProgresjon
                , samtale = { aktivSamtale = Introduksjon, historikk = [] }
                }

        _ ->
            Loading (VenterPåResten state)



--- UPDATE ---


type Msg
    = LoadingMsg LoadingMsg
    | SuccessMsg SuccessMsg


type SuccessMsg
    = BrukerSierHeiIIntroduksjonen
    | OriginalPersonaliaBekreftet
    | BrukerVilEndreOriginalPersonalia


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

        SuccessMsg successMsg ->
            case model of
                Success successModel ->
                    updateSuccessModel successMsg successModel

                _ ->
                    ( model, Cmd.none )


updateSuccessModel : SuccessMsg -> SuccessModel -> ( Model, Cmd Msg )
updateSuccessModel successMsg model =
    case successMsg of
        BrukerSierHeiIIntroduksjonen ->
            ( nesteSamtaleSteg model "Hei!" (PersonaliaSeksjon (BekreftOriginal model.personalia)) |> Success, Cmd.none )

        OriginalPersonaliaBekreftet ->
            ( nesteSamtaleSteg model "Bekreft" (PersonaliaSeksjon (BekreftOriginal model.personalia)) |> Success, Cmd.none )

        BrukerVilEndreOriginalPersonalia ->
            ( nesteSamtaleSteg model "Endre" (PersonaliaSeksjon (BekreftOriginal model.personalia)) |> Success, Cmd.none )


nesteSamtaleSteg : SuccessModel -> String -> SamtaleSeksjon -> SuccessModel
nesteSamtaleSteg ({ samtale } as model) tekst samtaleSeksjon =
    { model
        | samtale =
            { samtale
                | aktivSamtale = samtaleSeksjon
                , historikk = model.samtale.historikk ++ [ samtaleTilBoble model.samtale.aktivSamtale, Bruker tekst ]
            }
    }


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
        [ viewHistorikk successModel.samtale.historikk
        , viewAktivSamtale successModel.samtale.aktivSamtale
        ]


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
            personaliaSamtaleTilBoble personaliaSeksjon

        _ ->
            Robot "Ikke implementert"


personaliaSamtaleTilBoble : PersonaliaSeksjon -> Snakkeboble
personaliaSamtaleTilBoble personaliaSeksjon =
    case personaliaSeksjon of
        BekreftOriginal personalia ->
            Robot
                ("Jeg har hentet inn kontaktinformasjonen din. Den vil vises på CV-en."
                    ++ "Det er viktig at informasjonen er riktig, slik at arbeidsgivere kan kontakte deg. "
                    ++ "Fornavn: "
                    ++ (Personalia.fornavn personalia |> Maybe.withDefault "-")
                    ++ " Etternavn: "
                    ++ (Personalia.etternavn personalia |> Maybe.withDefault "-")
                )

        EndreOriginal personaliaSkjema ->
            Robot "Endre personalia"


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
            viewBrukerInputPersonalia personaliaSeksjon

        _ ->
            text ""


viewBrukerInputPersonalia : PersonaliaSeksjon -> Html Msg
viewBrukerInputPersonalia personaliaSeksjon =
    case personaliaSeksjon of
        BekreftOriginal personalia ->
            div []
                [ button [ onClick (SuccessMsg BrukerSierHeiIIntroduksjonen) ] [ text "Endre" ]
                , button [ onClick (SuccessMsg BrukerSierHeiIIntroduksjonen) ] [ text "Bekreft" ]
                ]

        EndreOriginal personaliaSkjema ->
            text ""



{-

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

   -
-}
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
