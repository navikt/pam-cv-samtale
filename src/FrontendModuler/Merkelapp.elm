module FrontendModuler.Merkelapp exposing (Merkelapp, merkelapp, merkelapper)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type Merkelapp msg
    = Merkelapp (Options msg)


type alias Options msg =
    { msg : msg
    , tekst : String
    }


merkelapp : msg -> String -> Merkelapp msg
merkelapp msg_ tekst_ =
    Merkelapp
        { msg = msg_
        , tekst = tekst_
        }


merkelapper : List (Merkelapp msg) -> Html msg
merkelapper merkelappListe =
    div [ class "Merkelapp__wrapper" ]
        (List.map merkelappToHtml merkelappListe)


merkelappToHtml : Merkelapp msg -> Html msg
merkelappToHtml (Merkelapp options) =
    button
        [ class "Merkelapp typo-element"
        , onClick options.msg
        ]
        [ div [ class "Merkelapp__text" ] [ text options.tekst ]
        , i [ class "Merkelapp__slett__icon" ] []
        ]
