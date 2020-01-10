module FrontendModuler.LoggInnLenke exposing (loggInnLenkeTekst, viewLoggInnLenke)

import FrontendModuler.BrukerInput as BrukerInput exposing (BrukerInput)
import FrontendModuler.Lenke as Lenke


viewLoggInnLenke : BrukerInput msg
viewLoggInnLenke =
    BrukerInput.lenke
        (Lenke.lenke { tekst = loggInnLenkeTekst, url = "/cv-samtale/login?redirect=/logget-inn" }
            |> Lenke.withTargetBlank
            |> Lenke.withId "logg-inn-lenke-id"
        )


loggInnLenkeTekst : String
loggInnLenkeTekst =
    "Ja, jeg vil logge inn"
