module Validering exposing (feilmeldingMaxAntallTegn)


feilmeldingMaxAntallTegn : String -> Int -> Maybe String
feilmeldingMaxAntallTegn innhold lengde =
    if String.length innhold <= lengde then
        Nothing

    else
        let
            tallTekst =
                (String.length innhold - lengde)
                    |> String.fromInt
        in
        Just ("Du har " ++ tallTekst ++ " tegn for mye")
