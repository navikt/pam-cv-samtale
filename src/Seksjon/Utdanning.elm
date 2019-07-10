module Seksjon.Utdanning exposing (Model, ModelInfo, Msg, Samtale(..), SamtaleStatus(..), historikk, init, update, viewUtdanning)

import Cv.Utdanning as Cv exposing (Utdanning)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Personalia exposing (Personalia)
import Skjema.Utdanning exposing (UtdanningSkjema)
import Snakkeboble exposing (Snakkeboble(..))



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { historikk : List Snakkeboble, aktivSamtale : Samtale, utdanningListe : List Utdanning, nivå : String }


type Samtale
    = Intro
    | FullførtRegistrering
    | RegistrerUtdanning
    | RegistrerNivå String
    | RegistrerRetning NivåInfo
    | RegistrerSkole NivåRetningInfo
    | RegistrerBeskrivelse NivåRetningSkoleInfo
    | RegistrerPeriode NivåRetningSkoleBeskrivelseInfo
    | Oppsummering NivåRetningSkoleBeskrivelsePeriodeInfo


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


type Msg
    = BrukerVilRegistrereNyUtdanning
    | GåTilArbeidserfaring
    | BekreftAlleredeRegistrert
    | BrukerVilRegistrerNivå String


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig (List Utdanning) (List Snakkeboble)



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
            Ferdig model.utdanningListe model.historikk

        BekreftAlleredeRegistrert ->
            ( model.nivå
                |> RegistrerNivå
                |> nesteSamtaleSteg model "Endre"
            , Cmd.none
            )
                |> IkkeFerdig



{--Ferdig model.utdanning model.historikk--}


historikk : Model -> List Snakkeboble
historikk (Model model) =
    model.historikk ++ [ samtaleTilBoble model.aktivSamtale ]


nesteSamtaleSteg : ModelInfo -> String -> Samtale -> Model
nesteSamtaleSteg model tekst samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , historikk = model.historikk ++ [ samtaleTilBoble model.aktivSamtale, samtaleTilBoble samtaleSeksjon, Bruker tekst ]
        }


oppdaterSamtaleSteg : ModelInfo -> Samtale -> Model
oppdaterSamtaleSteg model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , historikk = model.historikk ++ [ samtaleTilBoble samtaleSeksjon ]
        }


samtaleTilBoble : Samtale -> Snakkeboble
samtaleTilBoble utdanningSeksjon =
    case utdanningSeksjon of
        Intro ->
            Robot "Nå skal jeg spise barna dine!"

        FullførtRegistrering ->
            Robot "Gå videre"

        RegistrerUtdanning ->
            Robot "Gå videre"

        RegistrerNivå tekst ->
            Robot ("Du har valgt nivå " ++ tekst)

        RegistrerRetning tekst ->
            Robot ("Du har valgt nivå " ++ tekst)

        RegistrerSkole ->
            Robot "Gå videre"

        RegistrerBeskrivelse ->
            Robot "Gå videre"

        RegistrerPeriode ->
            Robot "Gå videre"

        Oppsummering ->
            Robot "Gå videre"


viewUtdanning : Model -> Html Msg
viewUtdanning (Model { aktivSamtale }) =
    case aktivSamtale of
        Intro ->
            div []
                [ text "Hei, nå skal vi legge til utdanningen din"
                , button [ onClick VilRegistrereNyUtdanning ] [ text "Jeg vil registrere utdanning" ]
                , button [ onClick GåTilArbeidserfaring ] [ text "Jeg har ingen utdanning" ]
                ]

        FullførtRegistrering ->
            div [] []

        RegistrerUtdanning ->
            div [] []

        RegistrerNivå nivå ->
            div [] []

        RegistrerRetning ->
            div [] []

        RegistrerSkole ->
            div [] []

        RegistrerBeskrivelse ->
            div [] []

        RegistrerPeriode ->
            div [] []

        Oppsummering ->
            div [] []



--- INIT ---


init : List Utdanning -> Model
init utdanningListe =
    Model { historikk = [], aktivSamtale = Intro, utdanningListe = utdanningListe, nivå = "" }
