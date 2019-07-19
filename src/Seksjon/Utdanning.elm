module Seksjon.Utdanning exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Cv.Utdanning as Cv exposing (Utdanning)
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (MeldingsLogg)
import Personalia exposing (Personalia)
import Skjema.Utdanning exposing (UtdanningSkjema)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , utdanningListe : List Utdanning
    }


type alias NivåInfo =
    -- TODO: legg til custom type
    { nivå : String }


type alias RetningInfo =
    { nivå : String, retning : String }


type alias SkoleInfo =
    { nivå : String, retning : String, skole : String }


type alias BeskrivelseInfo =
    { nivå : String, retning : String, skole : String, beskrivelse : String }


type alias PeriodeInfo =
    { nivå : String, retning : String, skole : String, beskrivelse : String, periode : String }


type alias OppsummeringInfo =
    { nivå : String, retning : String, skole : String, beskrivelse : String, periode : String, oppsummering : String }


type Samtale
    = Intro
    | RegistrerUtdanning
    | RegistrerNivå NivåInfo
    | RegistrerRetning RetningInfo
    | RegistrerSkole SkoleInfo
    | RegistrerBeskrivelse BeskrivelseInfo
    | RegistrerPeriode PeriodeInfo
    | Oppsummering OppsummeringInfo


type Msg
    = BrukerVilRegistrereNyUtdanning
    | GåTilArbeidserfaring
    | BekreftAlleredeRegistrert
    | BrukerVilRegistrerNivå String


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Utdanning) MeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilRegistrereNyUtdanning ->
            case model.aktivSamtale of
                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        GåTilArbeidserfaring ->
            Ferdig model.utdanningListe model.seksjonsMeldingsLogg

        _ ->
            IkkeFerdig ( Model model, Cmd.none )


nesteSamtaleSteg : ModelInfo -> String -> Samtale -> Model
nesteSamtaleSteg model tekst samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
        }


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg utdanningSeksjon =
    case utdanningSeksjon of
        Intro ->
            [ Melding.spørsmål
                [ "Nå skal vi legge til din utdanning" ]
            ]

        RegistrerUtdanning ->
            [ Melding.spørsmål
                [ "Du har valgt å registrere utdanning" ]
            ]

        _ ->
            [ Melding.spørsmål
                [ "Diverse valg" ]
            ]


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model { aktivSamtale }) =
    case aktivSamtale of
        Intro ->
            div [ class "inputrad" ]
                [ div [ class "inputrad-innhold" ]
                    [ Knapp.knapp BrukerVilRegistrereNyUtdanning "Jeg vil registrere utdanning"
                        |> Knapp.toHtml
                    , Knapp.knapp GåTilArbeidserfaring "Jeg har ingen utdanning"
                        |> Knapp.toHtml
                    ]
                ]

        RegistrerUtdanning ->
            div [] []

        RegistrerNivå nivå ->
            div [] []

        RegistrerRetning nivåInfo ->
            div [] []

        RegistrerSkole nivåRetningInfo ->
            div [] []

        RegistrerBeskrivelse nivåRetningSkoleInfo ->
            div [] []

        RegistrerPeriode nivåRetningSkoleBeskrivelseInfo ->
            div [] []

        Oppsummering nivåRetningSkoleBeskrivelsePeriodeInfo ->
            div [] []



--- INIT ---


init : MeldingsLogg -> List Utdanning -> Model
init gammelMeldingsLogg utdanningListe =
    let
        aktivSamtale =
            Intro
    in
    Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål
                (samtaleTilMeldingsLogg aktivSamtale)
                gammelMeldingsLogg
        , aktivSamtale = aktivSamtale
        , utdanningListe = utdanningListe
        }
