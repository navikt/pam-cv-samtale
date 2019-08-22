module Melding exposing (Melding, innhold, spørsmål, svar)


type Melding
    = Melding (List String)


spørsmål : List String -> Melding
spørsmål list =
    Melding list


svar : List String -> Melding
svar list =
    Melding list


innhold : Melding -> List String
innhold (Melding list) =
    list
