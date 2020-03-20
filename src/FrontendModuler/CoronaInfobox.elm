module FrontendModuler.CoronaInfobox exposing (CoronaInfobox, coronaInfobox, toHtml)

import Html exposing (Html, div, h2, section, text)
import Html.Attributes exposing (class)
import Html.Attributes.Aria exposing (ariaLabel)


type CoronaInfobox msg
    = CoronaInfobox
        { title : String
        , message : String
        }


coronaInfobox : String -> String -> CoronaInfobox msg
coronaInfobox title message =
    CoronaInfobox { title = title, message = message }


toHtml : CoronaInfobox msg -> Html msg
toHtml (CoronaInfobox options) =
    div [ class "corona-infobox" ]
        [ div [ class "corona-infobox__header" ]
            [ div [ class "corona-infobox__circle", ariaLabel "Viktig" ] []
            , h2 [] [ text options.title ]
            ]
        , section []
            [ text options.message
            ]
        ]
