module AndreSider.Inngang exposing (..)

import AndreSider.HeaderInfo as HeaderInfo exposing (HeaderInfo)
import Api
import Browser
import Browser.Navigation as Navigation
import DebugStatus
import Feilmelding
import FrontendModuler.ArbeidsplassenHeader as ArbeidsplassenHeader
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Lenkepanel as Lenkepanel
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error(..))
import Main
import Melding
import MeldingsLogg exposing (MeldingsLogg)
import SamtaleAnimasjon



--- MODEL ---


type Model
    = Loading
    | Success
        { meldingslogg : MeldingsLogg
        , headerInfo : HeaderInfo
        , headerOpen : Bool
        }
    | Failure Http.Error



--- UPDATE ---


type Msg
    = HeaderInfoHentet (Result Http.Error HeaderInfo)
    | ToogleHeaderOpen Bool
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg
    | ErrorLogget (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HeaderInfoHentet result ->
            case result of
                Ok headerInfo ->
                    ( Success
                        { headerInfo = headerInfo
                        , headerOpen = False
                        , meldingslogg =
                            MeldingsLogg.init
                                |> MeldingsLogg.leggTilSpørsmål
                                    [ Melding.spørsmål [ "Hei " ++ HeaderInfo.fornavn headerInfo ++ ". Jeg er en robot som gjerne vil hjelpe deg med CV-en. Du får tips og eksempler underveis." ]
                                    , Melding.spørsmål [ "Vil du ha hjelp av meg, eller vil du fylle ut CV-en selv?" ]
                                    ]
                        }
                    , DebugStatus.regular
                        |> SamtaleAnimasjon.startAnimasjon
                        |> Cmd.map SamtaleAnimasjonMsg
                    )

                Err error ->
                    case error of
                        BadStatus 401 ->
                            ( model, Navigation.load "/cv-samtale/login?redirect=/cv-valg" )

                        _ ->
                            ( Failure error
                            , error
                                |> Feilmelding.feilmelding "Hent headerInfo i inngang"
                                |> Maybe.map (Api.logError ErrorLogget)
                                |> Maybe.withDefault Cmd.none
                            )

        ToogleHeaderOpen open ->
            case model of
                Success successModel ->
                    ( Success { successModel | headerOpen = open }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SamtaleAnimasjonMsg samtaleAnimasjonMsg ->
            case model of
                Success successModel ->
                    let
                        ( nyMeldingslogg, cmd ) =
                            SamtaleAnimasjon.update DebugStatus.regular samtaleAnimasjonMsg successModel.meldingslogg
                    in
                    ( Success { successModel | meldingslogg = nyMeldingslogg }
                    , cmd
                        |> Cmd.map SamtaleAnimasjonMsg
                    )

                _ ->
                    ( model, Cmd.none )

        ErrorLogget _ ->
            ( model, Cmd.none )



--- VIEW ---


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            text ""

        Success successModel ->
            div [ class "inngang" ]
                [ { navn = HeaderInfo.navn successModel.headerInfo
                  , underOppfølging = HeaderInfo.underOppfølging successModel.headerInfo
                  , open = successModel.headerOpen
                  , onOpenToggle = ToogleHeaderOpen
                  }
                    |> ArbeidsplassenHeader.header
                    |> ArbeidsplassenHeader.toHtml
                , div [ id "samtale" ]
                    [ div [ id "samtale-innhold" ]
                        [ div [ class "inngang-header-wrapper" ]
                            [ div [ class "inngang-header-logo-wrapper" ]
                                [ i [ class "Robotlogo-inngang-header" ] [] ]
                            , div [ class "inngang-header" ]
                                [ h1 [] [ text "Lag en CV" ]
                                ]
                            ]
                        , div [ class "samtale-wrapper" ]
                            [ div [ class "samtale inngang--samtale" ]
                                [ Main.viewMeldingsLogg successModel.meldingslogg
                                , viewBrukerInput successModel.meldingslogg
                                ]
                            ]
                        ]
                    ]
                ]

        Failure _ ->
            text ""


viewBrukerInput : MeldingsLogg -> Html msg
viewBrukerInput meldingsLogg =
    if MeldingsLogg.visBrukerInput meldingsLogg then
        Containers.knapper Flytende
            [ Lenkepanel.lenkepanel { tekst = "Jeg vil ha hjelp av roboten", url = "/cv-valg/samtale" }
                |> Lenkepanel.withClass "inngang-lenkepanel"
                |> Lenkepanel.toHtml
            , Lenkepanel.lenkepanel { tekst = "Jeg vil fylle ut CV-en selv", url = "/cv-valg/skjema" }
                |> Lenkepanel.withClass "inngang-lenkepanel"
                |> Lenkepanel.toHtml
            ]

    else
        text ""



--- PROGRAM ---


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Api.getHeaderInfo HeaderInfoHentet )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Success { meldingslogg } ->
            meldingslogg
                |> SamtaleAnimasjon.subscriptions
                |> Sub.map SamtaleAnimasjonMsg

        _ ->
            Sub.none
