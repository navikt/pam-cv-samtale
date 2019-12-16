module FrontendModuler.ManedKnapper exposing (månedKnapper)

import Dato.Maned as Måned exposing (Måned)
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)


månedKnapper : (Måned -> msg) -> Html msg
månedKnapper onMånedClick =
    div [ class "knapperad" ]
        [ div [ class "knapper--måneder" ]
            (List.map (månedKnapp onMånedClick) Måned.måneder)
        ]


månedKnapp : (Måned -> msg) -> Måned -> Html msg
månedKnapp onMånedClick måned =
    måned
        |> Måned.tilString
        |> Knapp.knapp (onMånedClick måned)
        |> Knapp.toHtml
