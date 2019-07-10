module Seksjon.Utdanning exposing (Model, ModelInfo, Msg, Samtale(..), SamtaleStatus(..), init, update, viewUtdanning)

import Cv.Utdanning as Cv exposing (Utdanning)
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
    { seksjonsMeldingsLogg : MeldingsLogg, aktivSamtale : Samtale, utdanningListe : List Utdanning, nivå : String }


type Samtale
    = Intro
    | FullførtRegistrering
    | RegistrerUtdanning
    | RegistrerNivå NivåInfo
    | RegistrerRetning NivåRetningInfo
    | RegistrerSkole NivåRetningSkoleInfo
    | RegistrerBeskrivelse NivåRetningSkoleBeskrivelseInfo
    | RegistrerPeriode NivåRetningSkoleBeskrivelsePeriodeInfo
    | Oppsummering NivåRetningSkoleBeskrivelsePeriodeOppsummeringInfo


type alias NivåInfo =
    { nivå : String }


type alias NivåRetningInfo =
    { nivå : String, retning : String }


type alias NivåRetningSkoleInfo =
    { nivå : String, retning : String, skole : String }


type alias NivåRetningSkoleBeskrivelseInfo =
    { nivå : String, retning : String, skole : String, beskrivelse : String }


type alias NivåRetningSkoleBeskrivelsePeriodeInfo =
    { nivå : String, retning : String, skole : String, beskrivelse : String, periode : String }


type alias NivåRetningSkoleBeskrivelsePeriodeOppsummeringInfo =
    { nivå : String, retning : String, skole : String, beskrivelse : String, periode : String, oppsummering : String }


type Msg
    = BrukerVilRegistrereNyUtdanning
    | GåTilArbeidserfaring
    | BekreftAlleredeRegistrert
    | BrukerVilRegistrerNivå String


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Utdanning) MeldingsLogg



--- UPDATE ---


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        BrukerVilRegistrereNyUtdanning ->
            case model.aktivSamtale of
                FullførtRegistrering ->
                    IkkeFerdig ( Model model, Cmd.none )

                _ ->
                    IkkeFerdig ( Model model, Cmd.none )

        {--
            ( model.aktivSamtale
                |> RegistrerNivå tekst
                |> nesteSamtaleSteg model "Endre"
            , Cmd.none
            )
                |> IkkeFerdig
--}
        GåTilArbeidserfaring ->
            Ferdig model.utdanningListe model.seksjonsMeldingsLogg

        _ ->
            IkkeFerdig ( Model model, Cmd.none )



{--Ferdig model.utdanning model.meldingsLogg--}


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


samtaleTilMeldingsLogg : Samtale -> ModelInfo -> MeldingsLogg
samtaleTilMeldingsLogg utdanningSeksjon model =
    case utdanningSeksjon of
        Intro ->
            MeldingsLogg.leggTilSpørsmål model.seksjonsMeldingsLogg
                [ Melding.spørsmål
                    [ "Intro" ]
                ]

        FullførtRegistrering ->
            MeldingsLogg.leggTilSpørsmål model.seksjonsMeldingsLogg
                [ Melding.spørsmål
                    [ "Du har valgt å endre personaliaskjema" ]
                ]

        RegistrerUtdanning ->
            MeldingsLogg.leggTilSpørsmål model.seksjonsMeldingsLogg
                [ Melding.spørsmål
                    [ "Du har valgt å endre personaliaskjema" ]
                ]

        _ ->
            MeldingsLogg.leggTilSpørsmål model.seksjonsMeldingsLogg
                [ Melding.spørsmål
                    [ "Diverse valg" ]
                ]


viewUtdanning : Model -> Html Msg
viewUtdanning (Model { aktivSamtale }) =
    case aktivSamtale of
        Intro ->
            div []
                [ text "Hei, nå skal vi legge til utdanningen din"
                , button [ onClick BrukerVilRegistrereNyUtdanning ] [ text "Jeg vil registrere utdanning" ]
                , button [ onClick GåTilArbeidserfaring ] [ text "Jeg har ingen utdanning" ]
                ]

        FullførtRegistrering ->
            div [] []

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
    Model { seksjonsMeldingsLogg = gammelMeldingsLogg, aktivSamtale = Intro, utdanningListe = utdanningListe, nivå = "" }
