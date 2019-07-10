module Melding exposing (Melding(..), MeldingsType(..), innhold, spørsmål, svar)


type Melding
    = Melding MeldingsType


type MeldingsType
    = Spørsmål (List String)
    | Svar (List String)


spørsmål : List String -> Melding
spørsmål list =
    Melding (Spørsmål list)


svar : List String -> Melding
svar list =
    Melding (Svar list)


innhold : Melding -> List String
innhold (Melding melding) =
    case melding of
        Spørsmål list ->
            list

        Svar list ->
            list



-- meldingsType: Melding -> MeldingsType
-- TODO: this ^
