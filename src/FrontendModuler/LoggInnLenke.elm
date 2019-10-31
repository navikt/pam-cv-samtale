module FrontendModuler.LoggInnLenke exposing (loggInnLenkeTekst, viewLoggInnLenke)

import FrontendModuler.Containers as Containers
import FrontendModuler.Lenke as Lenke
import Html exposing (Html)


viewLoggInnLenke : Html msg
viewLoggInnLenke =
    Containers.lenke
        (Lenke.lenke { tekst = loggInnLenkeTekst, url = "/cv-samtale/login?redirect=/logget-inn" }
            |> Lenke.withTargetBlank
            |> Lenke.toHtml
        )


loggInnLenkeTekst : String
loggInnLenkeTekst =
    "Ja, jeg vil logge inn"
