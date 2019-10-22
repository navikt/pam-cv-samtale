module FrontendModuler.Lenke exposing (Lenke, lenke, toHtml, withTargetBlank)

import Html exposing (..)
import Html.Attributes exposing (..)


type Lenke
    = Lenke
        { tekst : String
        , url : String
        , åpneINyFane : Bool
        }


lenke : { tekst : String, url : String } -> Lenke
lenke { tekst, url } =
    Lenke
        { tekst = tekst
        , url = url
        , åpneINyFane = False
        }


withTargetBlank : Lenke -> Lenke
withTargetBlank (Lenke options) =
    Lenke { options | åpneINyFane = True }


toHtml : Lenke -> Html msg
toHtml (Lenke options) =
    if options.åpneINyFane then
        span [ class "ForlateSiden" ]
            [ a
                [ href options.url
                , class "lenke"
                , target "_blank"
                ]
                [ text options.tekst
                , i [ class "ForlateSiden__icon" ] []
                ]
            ]

    else
        a
            [ href options.url
            , class "lenke"
            ]
            [ text options.tekst ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []
