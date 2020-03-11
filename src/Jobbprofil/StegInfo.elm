module Jobbprofil.StegInfo exposing (..)

import Arbeidserfaring.Yrke exposing (Yrke)
import Jobbprofil.JobbprofilValg exposing (AnsettelsesForm, Arbeidstider, Omfang, Oppstart(..))
import Jobbprofil.Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade exposing (Omrade)
import Jobbprofil.Skjema as Skjema exposing (ValidertSkjema)


type alias YrkeStegInfo =
    { yrker : List Yrke
    , visFeilmelding : Bool
    }


type alias OmradeStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , visFeilmelding : Bool
    }


type alias OmfangStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List Omfang
    }


type alias ArbeidstidStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List Omfang
    , arbeidstider : List Arbeidstider
    }


type alias AnsettelsesformStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List Omfang
    , arbeidstider : List Arbeidstider
    , ansettelsesformer : List AnsettelsesForm
    }


type alias OppstartStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List Omfang
    , arbeidstider : List Arbeidstider
    , ansettelsesformer : List AnsettelsesForm
    , oppstart : Maybe Oppstart
    , visFeilmelding : Bool
    }


type alias KompetanseStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List Omfang
    , arbeidstider : List Arbeidstider
    , ansettelsesformer : List AnsettelsesForm
    , oppstart : Oppstart
    , kompetanser : List Kompetanse
    , visFeilmelding : Bool
    }


yrkeStegInfo : YrkeStegInfo
yrkeStegInfo =
    { yrker = []
    , visFeilmelding = False
    }


yrkerInfoTilOmradeInfo : YrkeStegInfo -> OmradeStegInfo
yrkerInfoTilOmradeInfo info =
    { yrker = info.yrker
    , omrader = []
    , visFeilmelding = False
    }


omraderInfoTilOmfangInfo : OmradeStegInfo -> OmfangStegInfo
omraderInfoTilOmfangInfo info =
    { yrker = info.yrker
    , omrader = info.omrader
    , omfanger = []
    }


omfangInfoTilArbeidstidInfo : OmfangStegInfo -> ArbeidstidStegInfo
omfangInfoTilArbeidstidInfo info =
    { yrker = info.yrker
    , omrader = info.omrader
    , omfanger = info.omfanger
    , arbeidstider = []
    }


arbeidstidInfoTilAnsettelsesformInfo : ArbeidstidStegInfo -> AnsettelsesformStegInfo
arbeidstidInfoTilAnsettelsesformInfo info =
    { yrker = info.yrker
    , omrader = info.omrader
    , omfanger = info.omfanger
    , arbeidstider = info.arbeidstider
    , ansettelsesformer = []
    }


ansettelsesformInfoTilOppstartInfo : AnsettelsesformStegInfo -> OppstartStegInfo
ansettelsesformInfoTilOppstartInfo info =
    { yrker = info.yrker
    , omrader = info.omrader
    , omfanger = info.omfanger
    , arbeidstider = info.arbeidstider
    , ansettelsesformer = info.ansettelsesformer
    , oppstart = Nothing
    , visFeilmelding = False
    }


oppstartInfoTilKompetanseInfo : Oppstart -> OppstartStegInfo -> KompetanseStegInfo
oppstartInfoTilKompetanseInfo oppstart info =
    { yrker = info.yrker
    , omrader = info.omrader
    , omfanger = info.omfanger
    , arbeidstider = info.arbeidstider
    , ansettelsesformer = info.ansettelsesformer
    , oppstart = oppstart
    , kompetanser = []
    , visFeilmelding = False
    }


kompetanseInfoTilSkjema : KompetanseStegInfo -> ValidertSkjema
kompetanseInfoTilSkjema info =
    Skjema.initValidertSkjema
        { yrker = info.yrker
        , omrader = info.omrader
        , omfanger = info.omfanger
        , arbeidstider = info.arbeidstider
        , ansettelsesformer = info.ansettelsesformer
        , oppstart = info.oppstart
        , kompetanser = info.kompetanser
        }
