module FrontendModuler.ManedKnapper exposing (månedKnapper)

import Dato.Maned as Måned exposing (Måned)
import FrontendModuler.Knapp as Knapp exposing (Type(..))
import Html exposing (..)
import Html.Attributes exposing (..)


månedKnapper : { onMånedValg : Måned -> msg, onAvbryt : msg } -> Html msg
månedKnapper { onMånedValg, onAvbryt } =
    div [ class "knapperad" ]
        [ div []
            [ div [ class "knapper--måneder" ]
                (List.map (månedKnapp onMånedValg) Måned.måneder)
            ]
        , Knapp.knapp onAvbryt "Avbryt"
            |> Knapp.withType Flat
            |> Knapp.toHtml
        ]


månedKnapp : (Måned -> msg) -> Måned -> Html msg
månedKnapp onMånedClick måned =
    måned
        |> Måned.tilString
        |> Knapp.knapp (onMånedClick måned)
        |> Knapp.toHtml
