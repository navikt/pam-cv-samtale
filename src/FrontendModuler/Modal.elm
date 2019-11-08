module FrontendModuler.Modal exposing (modal, toHtml)

import Html exposing (..)
import Html.Attributes exposing (class, id, tabindex)
import Html.Attributes.Aria exposing (ariaLabel, role)
import Html.Events exposing (onClick)


type Modal msg
    = Modal (Options msg)


type alias ModalInfo msg =
    { id : String
    , isOpen : Bool
    , onClose : msg
    , innhold : Html msg
    }


type alias Options msg =
    { id : String
    , isOpen : Bool
    , onClose : msg
    , innhold : Html msg
    }


modal : ModalInfo msg -> Modal msg
modal info =
    Modal
        { id = info.id
        , isOpen = info.isOpen
        , innhold = info.innhold
        , onClose = info.onClose
        }


toHtml : Modal msg -> Html msg
toHtml (Modal options) =
    case options.isOpen of
        True ->
            div [ class "modal__overlay" ]
                [ div [ id options.id, class "modal", tabindex -1, role "dialog", ariaLabel "Modal - Gi tilbakemelding" ]
                    [ options.innhold
                    , button
                        [ class "lukknapp lukknapp--overstHjorne modal__lukknapp--shake"
                        , onClick options.onClose
                        ]
                        [ text "Lukk modal" ]
                    ]
                ]

        False ->
            text ""
