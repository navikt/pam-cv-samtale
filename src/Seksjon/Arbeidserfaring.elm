module Seksjon.Arbeidserfaring exposing (Model, Msg, SamtaleStatus(..), init, update, viewBrukerInput)

import Api
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
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
    = HentAAregArbeidserfaring
    | HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | RegistrerArbeidsErfaring Samtale
    | SkjemaEndret Skjema.ArbeidserfaringSkjema.Felt String
    | BrukerVilLeggeTilArbeidserfaring
    | BrukerVilLegeTilJobbTittel ArbeidserfaringSkjema
    | BrukerVilLeggeTilBedriftsnavn ArbeidserfaringSkjema
    | BrukerVilLeggeTilSted ArbeidserfaringSkjema
    | BrukerVilLeggeTilBeskrivelse ArbeidserfaringSkjema


type Samtale
    = Intro
    | LeggeTilArbeidserfaring InputType ArbeidserfaringSkjema
    | RegistrerNyTittel ArbeidserfaringSkjema
    | RegistrerBedriftNavn ArbeidserfaringSkjema
    | RegistrerSted ArbeidserfaringSkjema
    | Registrerarbeidsoppgaver ArbeidserfaringSkjema
    | RegistrerPeriode ArbeidserfaringSkjema
    | LagreArbeidserfaring ArbeidserfaringSkjema


type InputType
    = TypeString
    | TypeBool


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

        SkjemaEndret felt string ->
            case info.aktivSamtale of
                LeggeTilArbeidserfaring inputType skjema ->
                    case inputType of
                        TypeString ->
                            ( Model
                                { info
                                    | aktivSamtale =
                                        Skjema.ArbeidserfaringSkjema.oppdaterStringFelt skjema felt string
                                            |> LeggeTilArbeidserfaring inputType
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                        TypeBool ->
                            ( Model
                                { info
                                    | aktivSamtale =
                                        Skjema.ArbeidserfaringSkjema.toggleBool skjema felt
                                            |> LeggeTilArbeidserfaring inputType
                                }
                            , Cmd.none
                            )
                                |> IkkeFerdig

                _ ->
                    ( Model info, Cmd.none )
                        |> IkkeFerdig

        BrukerVilLeggeTilArbeidserfaring ->
            ( Model
                { info
                    | aktivSamtale =
                        Skjema.ArbeidserfaringSkjema.tomtSkjema
                            |> LeggeTilArbeidserfaring TypeString
                }
            , Cmd.none
            )
                |> IkkeFerdig

        HentAAregArbeidserfaring ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        BrukerVilLegeTilJobbTittel skjema ->
            ( Model
                { info
                    | aktivSamtale =
                        skjema
                            |> RegistrerNyTittel
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilLeggeTilBedriftsnavn skjema ->
            ( Model
                { info
                    | aktivSamtale =
                        skjema
                            |> RegistrerBedriftNavn
                }
            , Cmd.none
            )
                |> IkkeFerdig

        BrukerVilLeggeTilSted skjema ->
            ( Model info, Cmd.none )
                |> IkkeFerdig

        BrukerVilLeggeTilBeskrivelse skjema ->
            ( Model info, Cmd.none )
                |> IkkeFerdig



--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model info) =
    case info.aktivSamtale of
        Intro ->
            div []
                [ button [ onClick BrukerVilLeggeTilArbeidserfaring ] [ text "Legg til arbeidserfaring" ]
                ]

        LeggeTilArbeidserfaring steg arbeidserfaringSkjema ->
            div []
                [ label [] [ text "Yrke" ]
                , input
                    [ Skjema.ArbeidserfaringSkjema.yrke arbeidserfaringSkjema |> value
                    , onInput (SkjemaEndret Skjema.ArbeidserfaringSkjema.Yrke)
                    ]
                    []
                , button [ onClick (BrukerVilLegeTilJobbTittel arbeidserfaringSkjema) ] [ text "Neste" ]
                ]

        RegistrerNyTittel arbeidserfaringSkjema ->
            div []
                [ label [] [ text "Jobbnavn du ønsker skal stå på CV'en" ]
                , input
                    [ Skjema.ArbeidserfaringSkjema.yrkeFritekst arbeidserfaringSkjema |> value
                    , onInput (SkjemaEndret Skjema.ArbeidserfaringSkjema.YrkeFritekst)
                    ]
                    []
                , button [ onClick (BrukerVilLegeTilJobbTittel arbeidserfaringSkjema) ] [ text "Neste" ]
                ]

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
