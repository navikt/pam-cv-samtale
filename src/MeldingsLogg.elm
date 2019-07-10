module MeldingsLogg exposing (MeldigsLoggInfo, MeldingsLogg(..), hentMeldinger, init, leggTilSpørsmål, leggTilSvar)

import Melding exposing (Melding)


type MeldingsLogg
    = MeldingsLogg MeldigsLoggInfo


type alias MeldigsLoggInfo =
    List Melding


init : MeldingsLogg
init =
    MeldingsLogg []


hentMeldinger : MeldingsLogg -> List Melding
hentMeldinger (MeldingsLogg meldingsLogg) =
    meldingsLogg


leggTilSpørsmål : MeldingsLogg -> List Melding -> MeldingsLogg
leggTilSpørsmål meldingsLogg nyeMeldinger =
    MeldingsLogg (hentMeldinger meldingsLogg ++ nyeMeldinger)


leggTilSvar : MeldingsLogg -> Melding -> MeldingsLogg
leggTilSvar meldingsLogg nyMelding =
    MeldingsLogg (hentMeldinger meldingsLogg ++ [ nyMelding ])
