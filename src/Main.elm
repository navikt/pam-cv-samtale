module Main exposing (main)

import Api
import Browser
import Cv.Cv as Cv exposing (Cv)
import Cv.Utdanning as Utdanning exposing (Utdanning)
import Feilmelding
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Melding exposing (Melding)
import MeldingsLogg exposing (MeldingsLogg)
import Personalia exposing (Personalia)
import Seksjon.Personalia
import Seksjon.Utdanning



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
    }


type SamtaleSeksjon
    = Introduksjon MeldingsLogg
    | PersonaliaSeksjon Seksjon.Personalia.Model
    | UtdanningSeksjon Seksjon.Utdanning.Model
    | ArbeidsErfaringSeksjon



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
    | IkkeGjørNoe


type SuccessMsg
    = BrukerSierHeiIIntroduksjonen
    | PersonaliaMsg Seksjon.Personalia.Msg
    | UtdanningsMsg Seksjon.Utdanning.Msg


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

        IkkeGjørNoe ->
            ( model, Cmd.none )


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
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
                , aktivSamtale = initialiserSamtale
                }

        _ ->
            Loading (VenterPåResten state)


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
            case model.aktivSamtale of
                Introduksjon meldingsLogg ->
                    ( meldingsLogg
                        |> MeldingsLogg.leggTilSvar (Melding.svar [ "Ja!" ])
                        |> Seksjon.Personalia.init model.personalia
                        |> PersonaliaSeksjon
                        |> oppdaterSamtaleSteg model
                        |> Success
                    , Cmd.none
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
                            personaliaFerdig model personalia personaliaMeldingsLogg

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
                            utdanningFerdig model utdanning meldingsLogg

                _ ->
                    ( Success model, Cmd.none )


oppdaterSamtaleSteg : SuccessModel -> SamtaleSeksjon -> SuccessModel
oppdaterSamtaleSteg model samtaleSeksjon =
    { model
        | aktivSamtale = samtaleSeksjon
    }


personaliaFerdig : SuccessModel -> Personalia -> MeldingsLogg -> ( Model, Cmd Msg )
personaliaFerdig model personalia personaliaMeldingsLogg =
    ( Success
        { model
            | aktivSamtale = UtdanningSeksjon (Seksjon.Utdanning.init personaliaMeldingsLogg (Cv.utdanning model.cv))
        }
    , Cmd.none
    )


utdanningFerdig : SuccessModel -> List Utdanning -> MeldingsLogg -> ( Model, Cmd Msg )
utdanningFerdig model utdanning utdanningMeldingsLogg =
    ( Success
        { model
            | aktivSamtale = ArbeidsErfaringSeksjon
        }
    , Cmd.none
    )



--- VIEW ---


view : Model -> Html Msg
view model =
    case model of
        Loading loadingState ->
            text "loading spinner"

        Success successModel ->
            viewSuccess successModel

        Failure error ->
            text "error"


meldingsLoggFraSeksjon : SuccessModel -> MeldingsLogg
meldingsLoggFraSeksjon successModel =
    case successModel.aktivSamtale of
        Introduksjon logg ->
            logg

        PersonaliaSeksjon model ->
            Seksjon.Personalia.meldingsLogg model

        UtdanningSeksjon model ->
            Seksjon.Utdanning.meldingsLogg model

        ArbeidsErfaringSeksjon ->
            MeldingsLogg.init


viewSuccess : SuccessModel -> Html Msg
viewSuccess successModel =
    div [ class "samtale-wrapper" ]
        [ div [ class "samtale" ]
            [ successModel
                |> meldingsLoggFraSeksjon
                |> viewMeldingsLogg
            , viewBrukerInput successModel.aktivSamtale
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


viewBrukerInput : SamtaleSeksjon -> Html Msg
viewBrukerInput aktivSamtale =
    case aktivSamtale of
        Introduksjon logg ->
            button
                [ onClick
                    (BrukerSierHeiIIntroduksjonen
                        |> SuccessMsg
                    )
                ]
                [ text "Ja!" ]

        PersonaliaSeksjon personaliaSeksjon ->
            personaliaSeksjon
                |> Seksjon.Personalia.viewBrukerInput
                |> Html.map (PersonaliaMsg >> SuccessMsg)

        UtdanningSeksjon utdanningSeksjon ->
            utdanningSeksjon
                |> Seksjon.Utdanning.viewBrukerInput
                |> Html.map (UtdanningsMsg >> SuccessMsg)

        _ ->
            text "Ikke implementert"



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
