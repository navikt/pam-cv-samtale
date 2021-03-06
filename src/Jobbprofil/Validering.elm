module Jobbprofil.Validering exposing (feilmeldingKompetanse, feilmeldingOmråde, feilmeldingOppstart, feilmeldingYrke)

import Arbeidserfaring.Yrke exposing (Yrke)
import Jobbprofil.JobbprofilValg exposing (Oppstart)
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


feilmeldingYrke : List Yrke -> Maybe String
feilmeldingYrke yrker =
    if List.length yrker == 0 then
        Just "Skriv inn et yrke eller en stilling. Velg fra listen med forslag som kommer opp."

    else
        Nothing


feilmeldingOppstart : Maybe Oppstart -> Maybe String
feilmeldingOppstart oppstart =
    case oppstart of
        Just _ ->
            Nothing

        Nothing ->
            Just "Velg når du kan begynne i ny jobb."
