module MeldingsLogg exposing (MeldingsLogg, init, leggTilSpørsmål, leggTilSvar, meldinger)

import Melding exposing (Melding)


type MeldingsLogg
    = MeldingsLogg MeldigsLoggInfo


type alias MeldigsLoggInfo =
    List Melding


init : MeldingsLogg
init =
    MeldingsLogg []


meldinger : MeldingsLogg -> List Melding
meldinger (MeldingsLogg meldingsLogg) =
    meldingsLogg


leggTilSpørsmål : List Melding -> MeldingsLogg -> MeldingsLogg
leggTilSpørsmål nyeMeldinger (MeldingsLogg meldingsliste) =
    MeldingsLogg (meldingsliste ++ nyeMeldinger)


leggTilSvar : Melding -> MeldingsLogg -> MeldingsLogg
leggTilSvar nyMelding (MeldingsLogg meldingsliste) =
    MeldingsLogg (meldingsliste ++ [ nyMelding ])
