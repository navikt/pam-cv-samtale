module FrontendModuler.Alertstripe exposing (Alertstripe, alertstripe, toHtml)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, kind)
import Html.Attributes.Aria exposing (ariaLabel)
import Svg exposing (..)
import Svg.Attributes exposing (d, fill, viewBox)


type Alertstripe msg
    = Alertstripe { children : List (Html msg) }


alertstripe : List (Html msg) -> Alertstripe msg
alertstripe children =
    Alertstripe { children = children }



-- Det har bare vært behov for Feil-varianten av Alertstripe, så det er den eneste som finnes,
-- men andre varianter kan legges til ved behov


toHtml : Alertstripe msg -> Html msg
toHtml (Alertstripe options) =
    div [ class "alertstripe arbeidsplassen-alertstripe alertstripe--feil" ]
        [ span [ ariaLabel "feil", class "alertstripe__ikon" ]
            [ svg [ attribute "focusable" "false", attribute "height" "1.5em", kind "feil-sirkel-fyll", viewBox "0 0 24 24", attribute "width" "1.5em" ]
                [ g [ fill "none", attribute "fill-rule" "evenodd" ]
                    [ path
                        [ d "M11.999 0C5.395 0 .013 5.372 0 11.976a11.923 11.923 0 0 0 3.498 8.493A11.925 11.925 0 0 0 11.977 24H12c6.603 0 11.986-5.373 12-11.978C24.013 5.406 18.64.012 11.999 0z"
                        , fill "#E02C2C"
                        , attribute "fill-rule" "nonzero"
                        ]
                        []
                    , path
                        [ d "M12 10.651l3.372-3.372a.954.954 0 1 1 1.349 1.35L13.349 12l3.372 3.372a.954.954 0 1 1-1.35 1.349L12 13.349 8.628 16.72a.954.954 0 1 1-1.349-1.35L10.651 12 7.28 8.628A.954.954 0 1 1 8.63 7.28L12 10.651z"
                        , fill "#FFF"
                        , attribute "fill-rule" "nonzero"
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "typo-normal alertstripe__tekst" ]
            options.children
        ]
