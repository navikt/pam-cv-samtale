module ErrorHandtering exposing
    ( OperasjonEtterError(..)
    , errorMelding
    , operasjonEtterError
    )

import Http
import Melding exposing (Melding)


errorMelding : { operasjon : String, error : Http.Error } -> Melding
errorMelding { operasjon, error } =
    case error of
        -- Selve URLen ikke gyldig (inneholder linjeskift eller lignende)
        Http.BadUrl string ->
            -- GiOpp
            Melding.spørsmål [ "Oi! Nå skjedde det en feil, jeg klarte ikke å " ++ operasjon ++ ". Jeg beklager 😔 Du må gå videre til neste seksjon." ]

        Http.Timeout ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Hmm, nå klarte jeg ikke å " ++ operasjon ++ ". 😔 Det kan ha skjedd noe galt hos oss, eller kanskje du ikke er på nett. Sjekk om du er koblet til Internett og prøv igjen." ]

        Http.NetworkError ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Hmm, nå klarte jeg ikke å " ++ operasjon ++ " 😔 Du er ikke på nett. Koble til Internett og prøv igjen." ]

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


operasjonEtterError : Http.Error -> OperasjonEtterError
operasjonEtterError error =
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
