module ErrorHandtering exposing
    ( OperasjonEtterError(..)
    , errorMelding
    , feilmeldingEtterErrorILoading
    , feilmeldingTypeahead
    , operasjonEtterError
    , prøvPåNyttEtterTypeaheadError
    )

import Http
import Meldinger.Melding as Melding exposing (Melding)


errorMelding : { operasjon : String, error : Http.Error } -> Melding
errorMelding { operasjon, error } =
    case error of
        -- Selve URLen ikke gyldig (inneholder linjeskift eller lignende)
        Http.BadUrl string ->
            -- GiOpp
            Melding.spørsmål [ "Oi! Nå skjedde det en feil, jeg klarte ikke å " ++ operasjon ++ ". Jeg beklager 😔 Du må gå videre til neste seksjon." ]

        Http.Timeout ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Hmm, nå klarte jeg ikke å " ++ operasjon ++ " 😕 Det kan ha skjedd noe galt hos oss, eller kanskje du ikke er på nett. Sjekk om du er koblet til Internett og prøv igjen." ]

        Http.NetworkError ->
            -- PrøvPåNytt
            Melding.spørsmål [ "Hmm, nå klarte jeg ikke å " ++ operasjon ++ " 😕 Du er ikke på nett. Koble til Internett og prøv igjen." ]

        Http.BadStatus 401 ->
            -- LoggInn
            Melding.spørsmål [ "Oi! Jeg klarte ikke å " ++ operasjon ++ ". Du har dessverre blitt logget ut. Jeg beklager 😔 Hvis du logger inn igjen, kan du fortsette der du slapp. Vil du logge inn?" ]

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


operasjonEtterError : Http.Error -> OperasjonEtterError
operasjonEtterError error =
    case error of
        -- Selve URLen ikke gyldig (inneholder linjeskift eller lignende)
        Http.BadUrl _ ->
            GiOpp

        Http.Timeout ->
            PrøvPåNytt

        Http.NetworkError ->
            PrøvPåNytt

        Http.BadStatus 401 ->
            LoggInn

        Http.BadStatus _ ->
            PrøvPåNytt

        Http.BadBody _ ->
            PrøvPåNytt


feilmeldingEtterErrorILoading : Http.Error -> String
feilmeldingEtterErrorILoading error =
    case error of
        Http.BadUrl _ ->
            "Det skjedde noe feil under lasting av siden."

        Http.Timeout ->
            "Det skjedde noe feil under lasting av siden. Kan det være at du ikke er koblet til internett?"

        Http.NetworkError ->
            "Det skjedde noe feil under lasting av siden. Kan det være at du ikke er koblet til internett?"

        Http.BadStatus _ ->
            "Det skjedde noe feil under lasting av siden."

        Http.BadBody _ ->
            "Det skjedde noe feil under lasting av siden."


feilmeldingTypeahead : Http.Error -> String
feilmeldingTypeahead error =
    case error of
        Http.BadUrl _ ->
            "Oi! Nå skjedde det en feil, jeg klarte ikke å hente forslag i søkefeltet. Jeg beklager 😔"

        Http.Timeout ->
            "Hmm, nå klarte jeg ikke å hente forslag i søkefeltet 😕 Det kan ha skjedd noe galt hos oss, eller kanskje du ikke er på nett. Sjekk om du er koblet til Internett og prøv igjen."

        Http.NetworkError ->
            "Hmm, nå klarte jeg ikke å hente forslag i søkefeltet 😕 Du er ikke på nett. Koble til Internett og prøv igjen."

        Http.BadStatus _ ->
            "Oi! Nå skjedde det en feil, jeg klarte ikke å hente forslag i søkefeltet. Jeg beklager 😔"

        Http.BadBody _ ->
            "Oi! Nå skjedde det en feil, jeg klarte ikke å hente forslag i søkefeltet. Jeg beklager 😔"


prøvPåNyttEtterTypeaheadError : Http.Error -> Bool
prøvPåNyttEtterTypeaheadError error =
    case error of
        Http.BadUrl _ ->
            False

        Http.Timeout ->
            True

        Http.NetworkError ->
            True

        Http.BadStatus _ ->
            True

        Http.BadBody _ ->
            False
