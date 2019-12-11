module Meldinger.SamtaleOppdatering exposing (SamtaleOppdatering(..))

import Meldinger.Melding exposing (Melding)


type SamtaleOppdatering msg
    = IngenNyeMeldinger
    | SvarFraMsg msg
    | ManueltSvar Melding
    | UtenSvar
