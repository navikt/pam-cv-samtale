module FrontendModuler.ManedKnapper exposing (månedKnapper)

import Dato exposing (Måned(..))
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)


månedKnapper : (Måned -> msg) -> Html msg
månedKnapper onMånedClick =
    div [ class "knapperad" ]
        [ div [ class "knapper--måneder" ]
            (List.map (månedKnapp onMånedClick)
                [ Januar
                , Februar
                , Mars
                , April
                , Mai
                , Juni
                , Juli
                , August
                , September
                , Oktober
                , November
                , Desember
                ]
            )
        ]


månedKnapp : (Måned -> msg) -> Måned -> Html msg
månedKnapp onMånedClick måned =
    måned
        |> Dato.månedTilString
        |> Knapp.knapp (onMånedClick måned)
        |> Knapp.toHtml
