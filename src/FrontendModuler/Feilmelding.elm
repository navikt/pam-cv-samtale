module FrontendModuler.Feilmelding exposing (htmlFeilmelding)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)


htmlFeilmelding : Maybe String -> Html msg
htmlFeilmelding feilmelding =
    case feilmelding of
        Just feilmelding_ ->
            div [ role "alert", ariaLive "assertive" ]
                [ div [ class "skjemaelement__feilmelding" ]
                    [ text feilmelding_ ]
                ]

        Nothing ->
            text ""
