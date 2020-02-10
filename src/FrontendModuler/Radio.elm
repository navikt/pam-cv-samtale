module FrontendModuler.Radio exposing
    ( Radio
    , radio
    , toHtml
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Radio msg
    = Radio
        { label : String
        , value : String
        , msg : msg
        , checked : Bool
        }


radio : String -> String -> msg -> Bool -> Radio msg
radio label value msg checked =
    Radio
        { label = label
        , value = value
        , msg = msg
        , checked = checked
        }


toHtml : Radio msg -> Html msg
toHtml (Radio radio_) =
    div
        [ class "skjemaelement skjemaelement--horisontal" ]
        [ input
            [ type_ "radio"
            , class "skjemaelement__input radio"
            , checked radio_.checked
            , onClick radio_.msg
            ]
            []

        --- TODO: htmlFor
        , label [ class "skjemaelement__label", class "skjemaelement--checkbox-label", onClick radio_.msg ] [ text radio_.label ]
        ]
