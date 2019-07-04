module Introduksjon exposing (IntroduksjonSamtale(..))

import Personalia exposing (Personalia)


type IntroduksjonSamtale
    = Intro
    | BekreftPersonalia Personalia
    | EndrePersonalia Personalia
