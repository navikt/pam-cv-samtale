module FrontendModuler.ManedKnapper exposing (månedKnapper)

import Dato.Maned as Måned exposing (Måned)
import FrontendModuler.Knapp as Knapp exposing (Type(..))
import Html exposing (..)
import Html.Attributes exposing (..)


månedKnapper : { onMånedValg : Måned -> msg, onAvbryt : msg, fokusId : String } -> Html msg
månedKnapper { onMånedValg, onAvbryt, fokusId } =
    div [ class "knapperad" ]
        [ div []
            [ div [ class "knapper--måneder" ]
                (List.map (månedKnapp fokusId onMånedValg) Måned.måneder)
            ]
        , Knapp.knapp onAvbryt "Avbryt"
            |> Knapp.withType Flat
            |> Knapp.toHtml
        ]


månedKnapp : String -> (Måned -> msg) -> Måned -> Html msg
månedKnapp fokusId onMånedClick måned =
    case måned of
        Måned.Januar ->
            måned
                |> Måned.tilString
                |> Knapp.knapp (onMånedClick måned)
                |> Knapp.withId fokusId
                |> Knapp.toHtml

        _ ->
            måned
                |> Måned.tilString
                |> Knapp.knapp (onMånedClick måned)
                |> Knapp.toHtml
