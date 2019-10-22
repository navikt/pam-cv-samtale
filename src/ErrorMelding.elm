module ErrorMelding exposing
    ( OperasjonEtterError(..)
    , errorMelding
    , errorOperasjon
    )

import Http
import Melding exposing (Melding)


errorMelding : { operasjon : String, error : Http.Error } -> Melding
errorMelding { operasjon, error } =
    case error of
        -- Selve URLen ikke gyldig (inneholder linjeskift eller lignende)
        Http.BadUrl string ->
            -- GiOpp
            Melding.spørsmål [ "Oi! Nå skjedde det en feil, som gjorde at jeg ikke klarte å " ++ operasjon ++ ". Det kommer dessverre ikke til å fungerere å prøve på nytt, så du må gå videre." ]

        Http.Timeout ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Hm, nå klarte jeg ikke å " ++ operasjon ++ ". Kan det være at du ikke er koblet til internett? Eventuelt så har det skjedd noe galt hos oss. Sjekk om du har internett, prøv igjen, og se om det fungerer." ]

        Http.NetworkError ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Hm, nå klarte jeg ikke å " ++ operasjon ++ ". Kan det være at du ikke er koblet til internett? Sjekk om du har internett, prøv igjen, og se om det fungerer." ]

        Http.BadStatus 401 ->
            -- LoggInn
            Melding.spørsmål [ "Oi! Jeg klarte ikke å " ++ operasjon ++ ". Du har dessverre blitt logget ut. Jeg beklager 😔 Hvis du logger inn igjen, kan du fortsette der du slapp. Vil du logge inn?" ]

        --        Http.BadStatus 406 ->
        --            -- GodtaSamtykket
        --            Melding.spørsmål [ "" ]
        Http.BadStatus other ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Oi! Jeg klarte ikke å " ++ operasjon ++ "." ]

        Http.BadBody string ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Oi, nå skjedde det noe rart da jeg prøvde å " ++ operasjon ++ ". Jeg tror jeg fikk det til, men jeg klarte ikke å tolke svaret jeg fikk fra serveren. Mest sannsynlig så gikk det bra, så det tryggeste er nok å bare gå videre." ]


type OperasjonEtterError
    = GiOpp
    | PrøvPåNytt
    | LoggInn



--    | GodtaSamtykket


errorOperasjon : Http.Error -> OperasjonEtterError
errorOperasjon error =
    case error of
        -- Selve URLen ikke gyldig (inneholder linjeskift eller lignende)
        Http.BadUrl string ->
            GiOpp

        Http.Timeout ->
            PrøvPåNytt

        Http.NetworkError ->
            PrøvPåNytt

        Http.BadStatus 401 ->
            LoggInn

        --
        --        Http.BadStatus 406 ->
        --            GodtaSamtykket
        Http.BadStatus other ->
            PrøvPåNytt

        Http.BadBody string ->
            PrøvPåNytt
