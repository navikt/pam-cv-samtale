module Seksjon.Arbeidserfaring exposing (Model, Msg, init)

import Api
import Browser.Events exposing (onClick)
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Html exposing (Html, button, div, text)
import Http
import MeldingsLogg exposing (MeldingsLogg)
import Skjema.ArbeidserfaringSkjema exposing (ArbeidserfaringSkjema)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , arbeidserfaringListe : List Arbeidserfaring
    , aktivSamtale : Samtale
    }


arbeidserfaringListe : Model -> List Arbeidserfaring
arbeidserfaringListe (Model info) =
    info.arbeidserfaringListe


aktivSamtale : Model -> Samtale
aktivSamtale (Model info) =
    info.aktivSamtale


hentAAregArbeidserfaring : Model -> Cmd Msg
hentAAregArbeidserfaring (Model info) =
    Api.hentAAreg HentetAAregArbeidserfaring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )



--- UPDATE ---


type Msg
    = HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | RegistrerArbeidsErfaring Samtale
    | SkjemaEndret Skjema.ArbeidserfaringSkjema.Felt


type Samtale
    = Intro
    | LeggeTilArbeidsErfaring
    | RegistrerYrke ArbeidserfaringSkjema
    | RegistrerNyTittle ArbeidserfaringSkjema
    | RegistrerBedriftNavn ArbeidserfaringSkjema
    | RegistrerSted ArbeidserfaringSkjema
    | Registrerarbeidsoppgaver ArbeidserfaringSkjema
    | RegistrerPeriode ArbeidserfaringSkjema
    | LagreArbeidserfaring ArbeidserfaringSkjema


update : Msg -> Model -> SamtaleStatus
update msg (Model info) =
    case msg of
        HentetAAregArbeidserfaring result ->
            case result of
                Ok arbeidserfaringFraAAreg ->
                    ( Model
                        { info
                            | arbeidserfaringListe =
                                arbeidserfaringFraAAreg ++ info.arbeidserfaringListe
                        }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Err error ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        RegistrerArbeidsErfaring registreringsMsg ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        SkjemaEndret felt ->
            ( Model info, Cmd.none )
                |> IkkeFerdig


updateRegistrering : Samtale -> Model -> SamtaleStatus
updateRegistrering registreringsMsg (Model info) =
    case registreringsMsg of
        Intro ->
            IkkeFerdig ( Model info, Cmd.none )

        LeggeTilArbeidsErfaring ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerYrke skjema ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerNyTittle skjema ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerBedriftNavn skjema ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerSted skjema ->
            IkkeFerdig ( Model info, Cmd.none )

        Registrerarbeidsoppgaver skjema ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerPeriode skjema ->
            IkkeFerdig ( Model info, Cmd.none )

        LagreArbeidserfaring skjema ->
            IkkeFerdig ( Model info, Cmd.none )



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model info) =
    case info.aktivSamtale of
        Intro ->
            div []
                [ button [ onClick LeggeTilArbeidsErfaring ] [ text "Legg til arbeidserfaring" ]
                ]

        LeggeTilArbeidsErfaring ->
            div [] []

        RegistrerYrke arbeidserfaringSkjema ->
            div [] []

        RegistrerNyTittle arbeidserfaringSkjema ->
            div [] []

        RegistrerBedriftNavn arbeidserfaringSkjema ->
            div [] []

        RegistrerSted arbeidserfaringSkjema ->
            div [] []

        Registrerarbeidsoppgaver arbeidserfaringSkjema ->
            div [] []

        RegistrerPeriode arbeidserfaringSkjema ->
            div [] []

        LagreArbeidserfaring arbeidserfaringSkjema ->
            div [] []


init : MeldingsLogg -> Model
init gammelMeldingsLogg =
    Model
        { seksjonsMeldingsLogg = gammelMeldingsLogg
        , arbeidserfaringListe = []
        , aktivSamtale = Intro
        }
