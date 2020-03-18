module FrontendModuler.Alertstripe exposing (Alertstripe, Variant, alertstripeFeil, alertstripeInfo, toHtml, withClass)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, kind)
import Html.Attributes.Aria exposing (ariaLabel)
import Svg exposing (..)
import Svg.Attributes exposing (d, fill, viewBox)


type Alertstripe msg
    = Alertstripe
        { children : List (Html msg)
        , variant : Variant
        , class : Maybe String
        }


type Variant
    = AlertStripeFeil
    | AlertStripeInfo


alertstripeInfo : List (Html msg) -> Alertstripe msg
alertstripeInfo children =
    Alertstripe { children = children, variant = AlertStripeInfo, class = Nothing }


alertstripeFeil : List (Html msg) -> Alertstripe msg
alertstripeFeil children =
    Alertstripe { children = children, variant = AlertStripeFeil, class = Nothing }


withClass : String -> Alertstripe msg -> Alertstripe msg
withClass class (Alertstripe options) =
    Alertstripe { options | class = Just class }



-- Det har bare vært behov for Feil-varianten av Alertstripe, så det er den eneste som finnes,
-- men andre varianter kan legges til ved behov


toHtml : Alertstripe msg -> Html msg
toHtml (Alertstripe options) =
    case options.variant of
        AlertStripeFeil ->
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

        AlertStripeInfo ->
            div
                [ class "alertstripe arbeidsplassen-alertstripe"
                , options.class
                    |> Maybe.map class
                    |> Maybe.withDefault (classList [])
                ]
                [ span [ ariaLabel "info", class "alertstripe__ikon" ]
                    [ svg [ attribute "focusable" "false", attribute "height" "1.5em", kind "advarsel-sirkel-fyll", viewBox "0 0 24 24", attribute "width" "1.5em" ]
                        [ g [ fill "none", attribute "fill-rule" "evenodd" ]
                            [ path
                                [ d "M 12.205 -0.004 l -0.214 0.002 a 12.225 12.225 0 0 0 -8.517 3.659 C 1.179 5.977 -0.053 9.013 0.002 12.208 c 0.115 6.613 5.296 11.793 11.795 11.793 l 0.212 -0.002 c 6.726 -0.116 12.105 -5.595 11.99 -12.21 C 23.883 5.178 18.702 -0.003 12.204 -0.003 Z"
                                , fill "#ffa733"
                                , attribute "fill-rule" "nonzero"
                                ]
                                []
                            , path
                                [ d "M 12 5 a 1 1 0 0 1 1 1 v 7 a 1 1 0 0 1 -2 0 V 6 a 1 1 0 0 1 1 -1 Z"
                                , fill "#3e3832"
                                , attribute "fill-rule" "nonzero"
                                ]
                                []
                            , path
                                [ d "M 12.027 19 H 12 A 1.499 1.499 0 0 1 11.973 16 L 12 16 a 1.501 1.501 0 0 1 0.027 3 Z"
                                , fill "#3e3832"
                                , attribute "fill-rule" "nonzero"
                                ]
                                []
                            ]
                        ]
                    ]
                , div [ class "typo-normal alertstripe__tekst" ]
                    options.children
                ]
