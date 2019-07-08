module Main exposing (main)

import Api
import Browser
import Cv.Cv as Cv exposing (Cv)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    | LagrerEndring PersonaliaSkjema
    | LagringFeilet Http.Error PersonaliaSkjema


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
    | PersonaliaSkjemaEndret Skjema.Personalia.Felt String
    | PersonaliaskjemaLagreknappTrykket
    | PersonaliaOppdatert (Result Http.Error Personalia)


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
            ( model.personalia
                |> Skjema.Personalia.init
                |> EndreOriginal
                |> PersonaliaSeksjon
                |> nesteSamtaleSteg model "Endre"
                |> Success
            , Cmd.none
            )

        PersonaliaSkjemaEndret felt string ->
            case model.samtale.aktivSamtale of
                PersonaliaSeksjon (EndreOriginal skjema) ->
                    ( Skjema.Personalia.oppdaterFelt felt skjema string
                        |> EndreOriginal
                        |> PersonaliaSeksjon
                        |> oppdaterSamtaleSteg model
                        |> Success
                    , Cmd.none
                    )

                _ ->
                    ( model |> Success, Cmd.none )

        PersonaliaskjemaLagreknappTrykket ->
            case model.samtale.aktivSamtale of
                PersonaliaSeksjon (EndreOriginal skjema) ->
                    ( skjema
                        |> LagrerEndring
                        |> PersonaliaSeksjon
                        |> nesteSamtaleSteg model "Det jeg fylte ut"
                        |> Success
                    , model.personalia
                        |> Personalia.id
                        |> Api.oppdaterPersonalia (PersonaliaOppdatert >> SuccessMsg) skjema
                    )

                _ ->
                    ( model |> Success, Cmd.none )

        PersonaliaOppdatert result ->
            case model.samtale.aktivSamtale of
                PersonaliaSeksjon (LagrerEndring skjema) ->
                    case result of
                        Ok personalia ->
                            ( Utdanning
                                |> nesteSamtaleSteg model "Ok"
                                |> Success
                            , Cmd.none
                            )

                        Err error ->
                            ( LagringFeilet error skjema
                                |> PersonaliaSeksjon
                                |> nesteSamtaleSteg model "Olø noe gikk galt ass brusjan"
                                |> Success
                            , Cmd.none
                            )

                _ ->
                    ( model |> Success, Cmd.none )


nesteSamtaleSteg : SuccessModel -> String -> SamtaleSeksjon -> SuccessModel
nesteSamtaleSteg ({ samtale } as model) tekst samtaleSeksjon =
    { model
        | samtale =
            { samtale
                | aktivSamtale = samtaleSeksjon
                , historikk = model.samtale.historikk ++ [ samtaleTilBoble model.samtale.aktivSamtale, Bruker tekst ]
            }
    }


oppdaterSamtaleSteg : SuccessModel -> SamtaleSeksjon -> SuccessModel
oppdaterSamtaleSteg ({ samtale } as model) samtaleSeksjon =
    { model
        | samtale =
            { samtale
                | aktivSamtale = samtaleSeksjon
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

        LagrerEndring personaliaSkjema ->
            Robot "Spinner"

        LagringFeilet error personaliaSkjema ->
            Robot "Lagring feilet"


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
                [ button [ onClick (SuccessMsg BrukerVilEndreOriginalPersonalia) ] [ text "Endre" ]
                , button [ onClick (SuccessMsg OriginalPersonaliaBekreftet) ] [ text "Bekreft" ]
                ]

        EndreOriginal personaliaSkjema ->
            div []
                [ label [] [ text "Fornavn" ]
                , input
                    [ Skjema.Personalia.fornavn personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Fornavn >> SuccessMsg)
                    ]
                    []
                , label [] [ text "Etternavn" ]
                , input
                    [ Skjema.Personalia.etternavn personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Etternavn >> SuccessMsg)
                    ]
                    []
                , label [] [ text "Fødselsdato" ]
                , input
                    [ Skjema.Personalia.fodselsdato personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Fodelsdato >> SuccessMsg)
                    ]
                    []
                , label [] [ text "Epost" ]
                , input
                    [ Skjema.Personalia.epost personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Epost >> SuccessMsg)
                    ]
                    []
                , label [] [ text "Telefon" ]
                , input
                    [ Skjema.Personalia.telefon personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Telefon >> SuccessMsg)
                    ]
                    []
                , label [] [ text "Gateadresse" ]
                , input
                    [ Skjema.Personalia.gateadresse personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Gateadresse >> SuccessMsg)
                    ]
                    []
                , label [] [ text "Postnummer" ]
                , input
                    [ Skjema.Personalia.postnummer personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Postnummer >> SuccessMsg)
                    ]
                    []
                , label [] [ text "Poststed" ]
                , input
                    [ Skjema.Personalia.poststed personaliaSkjema |> value
                    , onInput (PersonaliaSkjemaEndret Skjema.Personalia.Poststed >> SuccessMsg)
                    ]
                    []
                , button [ onClick (SuccessMsg PersonaliaskjemaLagreknappTrykket) ] [ text "Lagre" ]
                ]

        LagrerEndring personaliaSkjema ->
            text ""

        LagringFeilet error personaliaSkjema ->
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
