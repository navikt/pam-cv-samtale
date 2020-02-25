module Jobbprofil.Validering exposing (feilmeldingKompetanse, feilmeldingOmråde)

import Jobbprofil.Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade exposing (Omrade)


feilmeldingOmråde : List Omrade -> Maybe String
feilmeldingOmråde områder =
    if List.length områder == 0 then
        Just "Skriv inn et fylke eller en kommune. Velg fra listen med forslag som kommer opp."

    else
        Nothing


feilmeldingKompetanse : List Kompetanse -> Maybe String
feilmeldingKompetanse kompetanser =
    if List.length kompetanser == 0 then
        Just "Skriv inn et ord som beskriver kompetansen din. Velg fra listen med forslag som kommer opp."

    else
        Nothing
