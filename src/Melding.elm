module Melding exposing (Melding, MeldingsType(..), innhold, meldingsType, spørsmål, svar)


type Melding
    = Melding { avsnitt : List String, meldingsType : MeldingsType }


type MeldingsType
    = Spørsmål
    | Svar


spørsmål : List String -> Melding
spørsmål list =
    Melding { avsnitt = list, meldingsType = Spørsmål }


svar : List String -> Melding
svar list =
    Melding { avsnitt = list, meldingsType = Svar }


innhold : Melding -> List String
innhold (Melding melding) =
    melding.avsnitt


meldingsType : Melding -> MeldingsType
meldingsType (Melding melding) =
    melding.meldingsType
