module Jobbprofil.Jobbprofil exposing (..)

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Iso8601
import Jobbprofil.JobbprofilValg as JobbprofilValg exposing (AnsettelsesForm, Arbeidsdag, ArbeidstidOrdning, Arbeidstider(..), Arbeidstidspunkt, Omfang, Oppstart, decodeAnsettelsesform, decodeArbeidstider, decodeOmfang, decodeOppstart)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Jobbprofil.Stilling as Stilling
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Time exposing (Posix)


type Jobbprofil
    = Jobbprofil JobbprofilInfo


type alias JobbprofilInfo =
    { stillingliste : List Yrke
    , kompetanseliste : List Kompetanse
    , geografiliste : List Omrade
    , ansettelsesformliste : List AnsettelsesForm
    , arbeidstidspunkt : List Arbeidstider
    , arbeidsdager : List Arbeidstider
    , arbeidstidsordninger : List Arbeidstider
    , omfangsliste : List Omfang
    , oppstart : Maybe Oppstart
    , sistEndretDato : Maybe Posix
    }


stillingliste : Jobbprofil -> List Yrke
stillingliste (Jobbprofil info) =
    info.stillingliste


kompetanseliste : Jobbprofil -> List Kompetanse
kompetanseliste (Jobbprofil info) =
    info.kompetanseliste


geografiliste : Jobbprofil -> List Omrade
geografiliste (Jobbprofil info) =
    info.geografiliste


ansettelsesformliste : Jobbprofil -> List AnsettelsesForm
ansettelsesformliste (Jobbprofil info) =
    info.ansettelsesformliste


arbeidstider : Jobbprofil -> List Arbeidstider
arbeidstider (Jobbprofil info) =
    -- I jobbprofilsamtalen behandler vi dager/tidspunkt/og arbeidstodsordning i samme klump
    List.concat [ info.arbeidsdager, info.arbeidstidspunkt, info.arbeidstidsordninger ]


omfangsliste : Jobbprofil -> List Omfang
omfangsliste (Jobbprofil info) =
    info.omfangsliste


oppstart : Jobbprofil -> Maybe Oppstart
oppstart (Jobbprofil info) =
    info.oppstart


sistEndretDato : Jobbprofil -> Maybe Posix
sistEndretDato (Jobbprofil info) =
    info.sistEndretDato



---- Decoder ----


decode : Decoder Jobbprofil
decode =
    decodeJobbprofil
        |> map Jobbprofil


decodeJobbprofil : Decoder JobbprofilInfo
decodeJobbprofil =
    succeed JobbprofilInfo
        |> required "stillingliste" (list Stilling.decodeJobbprofilStilling)
        |> required "kompetanseliste" (list Kompetanse.decodeJobbprofilKompetanse)
        |> required "geografiliste" (list Omrade.decodeJobbprofilGeografi)
        |> required "ansettelsesformliste" (list JobbprofilValg.decodeAnsettelsesform)
        |> required "arbeidstidliste" (list JobbprofilValg.decodeArbeidstider)
        |> required "arbeidsdagerliste" (list JobbprofilValg.decodeArbeidstider)
        |> required "arbeidstidsordningliste" (list JobbprofilValg.decodeArbeidstider)
        |> required "omfangsliste" (list JobbprofilValg.decodeOmfang)
        |> required "oppstart" (nullable JobbprofilValg.decodeOppstart)
        |> required "sistEndretDato" (nullable Iso8601.decoder)
