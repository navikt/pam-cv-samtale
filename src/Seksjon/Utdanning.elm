module Seksjon.Utdanning exposing (Model, Msg, SamtaleStatus(..), init, meldingsLogg, update, viewBrukerInput)

import Cv.Utdanning as Cv exposing (Utdanning)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Melding exposing (Melding(..))
import MeldingsLogg exposing (MeldingsLogg)
import Personalia exposing (Personalia)
import Skjema.Personalia
import Skjema.Utdanning exposing (UtdanningSkjema)



--- MODEL ---


type Model
    = Model ModelInfo


type Nivå
    = Nivå { utdanningsnivå : Utdanningsnivå }


type Utdanningsnivå
    = Grunnskole
    | VideregåendeYrkesskole
    | Fagskole
    | Folkehøyskole
    | HøyereUtdanning1til4
    | HøyereUtdanning4pluss
    | Phd



{--onst gyldigeUtdanningsNivaa = [
       { value: 2, label: 'Grunnskole' },
       { value: 4, label: 'Videregående/Yrkesskole' },
       { value: 5, label: 'Fagskole' },
       { value: 3, label: 'Folkehøyskole' },
       { value: 6, label: 'Høyere utdanning, 1-4 år' },
       { value: 7, label: 'Høyere utdanning, 4+ år' },
       { value: 8, label: 'PhD' }--}


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , utdanningListe : List Utdanning
    }


type alias NivåInfo =
    -- TODO: legg til custom type
    { nivå : Nivå }


type alias RetningInfo =
    { nivå : Nivå, retning : String }


type alias SkoleInfo =
    { nivå : Nivå, retning : String, skole : String }


type alias BeskrivelseInfo =
    { nivå : Nivå, retning : String, skole : String, beskrivelse : String }


type alias PeriodeInfo =
    { nivå : Nivå, retning : String, skole : String, beskrivelse : String, periode : String }


type alias OppsummeringInfo =
    { nivå : Nivå, retning : String, skole : String, beskrivelse : String, periode : String, oppsummering : String }


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
                Intro ->
                    IkkeFerdig
                        ( nesteSamtaleSteg model (Melding.svar [ "Jeg vil registrere utdannning" ]) RegistrerUtdanning
                        , Cmd.none
                        )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        GåTilArbeidserfaring ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Jeg har ingen utdanning" ])
                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Da fortsetter vi med arbeidserfaringen din" ] ]
                |> Ferdig model.utdanningListe

        _ ->
            IkkeFerdig ( Model model, Cmd.none )


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
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
            div []
                [ button [ onClick BrukerVilRegistrereNyUtdanning ] [ text "Jeg vil registrere utdanning" ]
                , button [ onClick GåTilArbeidserfaring ] [ text "Jeg har ingen utdanning" ]
                ]

        RegistrerUtdanning ->
            div []
                [ label [] [ text "Nivå" ]
                , button [ onClick (BrukerVilRegistrerNivå "Videregående") ] [ text "Videregående" ]
                , button [ onClick (BrukerVilRegistrerNivå "Bachelor") ] [ text "Bachelor" ]
                , button [ onClick (BrukerVilRegistrerNivå "Master") ] [ text "Master" ]
                ]

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
