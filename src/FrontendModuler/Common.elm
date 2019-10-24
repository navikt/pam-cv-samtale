module FrontendModuler.Common exposing (viewLoggInnLenke)

import FrontendModuler.Containers as Containers
import FrontendModuler.Lenke as Lenke
import Html exposing (Html)


viewLoggInnLenke : Html msg
viewLoggInnLenke =
    Containers.lenke
        (Lenke.lenke { tekst = "Ja, jeg vil logge inn ", url = "/cv-samtale/login?redirect=/logget-inn" }
            |> Lenke.withTargetBlank
            |> Lenke.toHtml
        )
