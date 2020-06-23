module FrontendModuler.Feilmelding exposing (htmlFeilmelding)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)


htmlFeilmelding : Maybe String -> Html msg
htmlFeilmelding feilmelding =
    case feilmelding of
        Just feilmelding_ ->
            div [ role "alert", ariaLive "polite" ]
                [ div [ class "skjemaelement__feilmelding" ]
                    [ p [ class "typo-feilmelding" ] [ text feilmelding_ ] ]
                ]

        Nothing ->
            text ""
