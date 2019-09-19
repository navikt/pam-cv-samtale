module Melding exposing (Melding, innhold, spørsmål, svar, tomLinje)


type Melding
    = Melding (List String)


spørsmål : List String -> Melding
spørsmål list =
    Melding list


svar : List String -> Melding
svar list =
    Melding list


innhold : Melding -> List String
innhold (Melding linjer) =
    linjer
        |> List.map (String.split "\n")
        |> List.concat
        |> List.map
            (\linje ->
                if (String.trim >> String.isEmpty) linje then
                    tomLinje

                else
                    linje
            )


tomLinje : String
tomLinje =
    "\u{00A0}"
