module Seksjon.Arbeidserfaring exposing (Model, Msg)

import Api
import Cv.Arbeidserfaring exposing (Arbeidserfaring)
import Http
import Skjema.ArbeidserfaringSkjema



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { arbeidserfaringListe : List Arbeidserfaring
    , arbeidserfaringSomEndres : Arbeidserfaring
    , aktivSamtale : Samtale
    }


arbeidserfaringListe : Model -> List Arbeidserfaring
arbeidserfaringListe (Model info) =
    info.arbeidserfaringListe


aktivSamtale : Model -> Samtale
aktivSamtale (Model info) =
    info.aktivSamtale


arbeidserfaringSomEndres : Model -> Arbeidserfaring
arbeidserfaringSomEndres (Model info) =
    info.arbeidserfaringSomEndres


hentAAregArbeidserfaring : Model -> Cmd Msg
hentAAregArbeidserfaring (Model info) =
    Api.hentAAreg HentetAAregArbeidserfaring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )



--- UPDATE ---


type Msg
    = HentetAAregArbeidserfaring (Result Http.Error (List Arbeidserfaring))
    | RegistrerArbeidsErfaring Samtale


type Samtale
    = RegistrerYrke
    | RegistrerNyTittle
    | RegistrerBedriftNavn
    | RegistrerSted
    | Registrerarbeidsoppgaver
    | RegistrerPeriode Bool -- Nåværende?
    | LagreArbeidserfaring


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
            IkkeFerdig
                ( Model info
                , Cmd.none
                )


updateRegistrering : Samtale -> Model -> SamtaleStatus
updateRegistrering registreringsMsg (Model info) =
    case registreringsMsg of
        RegistrerYrke ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerNyTittle ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerBedriftNavn ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerSted ->
            IkkeFerdig ( Model info, Cmd.none )

        Registrerarbeidsoppgaver ->
            IkkeFerdig ( Model info, Cmd.none )

        RegistrerPeriode bool ->
            IkkeFerdig ( Model info, Cmd.none )

        LagreArbeidserfaring ->
            IkkeFerdig ( Model info, Cmd.none )
