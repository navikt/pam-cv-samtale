module Jobbprofil.StegInfo exposing (..)

import Arbeidserfaring.Yrke exposing (Yrke)
import Jobbprofil.Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade exposing (Omrade)


type alias YrkeStegInfo =
    { yrker : List Yrke
    , underOppfølging : Bool -- todo: fjern underOppfølging herfra
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
    , omfanger : List String
    }


type alias ArbeidstidStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List String
    , arbeidstider : List String
    }


type alias AnsettelsesformStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List String
    , arbeidstider : List String
    , ansettelsesformer : List String
    }


type alias OppstartStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List String
    , arbeidstider : List String
    , ansettelsesformer : List String
    , oppstart : String
    }


type alias KompetanseStegInfo =
    { yrker : List Yrke
    , omrader : List Omrade
    , omfanger : List String
    , arbeidstider : List String
    , ansettelsesformer : List String
    , oppstart : String
    , kompetanser : List Kompetanse
    , visFeilmelding : Bool
    }
