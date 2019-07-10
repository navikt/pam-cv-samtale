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

        _ ->
            IkkeFerdig ( Model model, Cmd.none )



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

        RegistrerNivå info ->
            Robot ("Du har valgt nivå " ++ info.nivå)

        RegistrerRetning info ->
            Robot ("Du har valgt nivå " ++ info.nivå ++ "retning: " ++ info.retning)

        RegistrerSkole info ->
            Robot ("Du har valgt nivå " ++ info.nivå ++ "retning: " ++ info.retning ++ "skole: " ++ info.skole)

        RegistrerBeskrivelse info ->
            Robot ("Du har valgt nivå " ++ info.nivå ++ "retning: " ++ info.retning ++ "skole: " ++ info.skole ++ "beskrivelse: " ++ info.beskrivelse)

        RegistrerPeriode info ->
            Robot ("Du har valgt nivå " ++ info.nivå ++ "retning: " ++ info.retning ++ "skole: " ++ info.skole ++ "beskrivelse: " ++ info.beskrivelse ++ "periode: " ++ info.periode)

        Oppsummering info ->
            Robot ("Du har valgt nivå " ++ info.nivå ++ "retning: " ++ info.retning ++ "skole: " ++ info.skole ++ "beskrivelse: " ++ info.beskrivelse ++ "periode: " ++ info.periode ++ "oppsummering: " ++ info.oppsummering)


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


init : List Utdanning -> Model
init utdanningListe =
    Model { historikk = [], aktivSamtale = Intro, utdanningListe = utdanningListe, nivå = "" }
