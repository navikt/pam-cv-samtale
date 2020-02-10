module Jobbprofil.Seksjon exposing (..)

import DebugStatus exposing (DebugStatus)
import Jobbprofil.Jobbprofil exposing (Jobbprofil)
import Meldinger.Melding exposing (Melding)
import Meldinger.MeldingsLogg as MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg)
import Meldinger.SamtaleAnimasjon as SamtaleAnimasjon
import Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))
import Time exposing (Posix)



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , jobbprofil : Jobbprofil
    , debugStatus : DebugStatus
    , sistLagretFraCV : Posix
    }


type Samtale
    = Intro Jobbprofil
    | BekreftJobbprofil


type FullføringStatus
    = LagringLyktesFørsteGang
    | LagringLyktesEtterFlereForsøk
    | BrukerGikkVidere


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Posix Jobbprofil FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = VilEndreJobbprofil
    | SamtaleAnimasjonMsg SamtaleAnimasjon.Msg


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg jobbprofilSamtale =
    case jobbprofilSamtale of
        Intro jobbprofil ->
            []

        _ ->
            []


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    SamtaleAnimasjon.startAnimasjon debugStatus
        |> Cmd.map SamtaleAnimasjonMsg



--- VIEW ---
--- INIT ---


init : DebugStatus -> Posix -> FerdigAnimertMeldingsLogg -> Jobbprofil -> ( Model, Cmd Msg )
init debugStatus sistLagretFraCV gammelMeldingsLogg jobbprofil =
    let
        aktivSamtale =
            Intro jobbprofil
    in
    ( Model
        { seksjonsMeldingsLogg =
            gammelMeldingsLogg
                |> MeldingsLogg.tilMeldingsLogg
                |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale)
        , aktivSamtale = aktivSamtale
        , jobbprofil = jobbprofil
        , debugStatus = debugStatus
        , sistLagretFraCV = sistLagretFraCV
        }
    , lagtTilSpørsmålCmd debugStatus
    )
