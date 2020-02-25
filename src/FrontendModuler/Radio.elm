module FrontendModuler.Radio exposing
    ( Radio
    , checkedRadioToString
    , radio
    , toHtml
    , withClass
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List


type Radio msg
    = Radio
        { label : String
        , value : String
        , msg : msg
        , checked : Bool
        , class : Maybe String
        }


radio : String -> String -> msg -> Bool -> Radio msg
radio label value msg checked =
    Radio
        { label = label
        , value = value
        , msg = msg
        , checked = checked
        , class = Nothing
        }


withClass : String -> Radio msg -> Radio msg
withClass class (Radio options) =
    Radio { options | class = Just class }


toHtml : Radio msg -> Html msg
toHtml (Radio options) =
    div
        [ class "skjemaelement skjemaelement--horisontal"
        , options.class
            |> Maybe.map class
            |> Maybe.withDefault noAttribute
        ]
        [ input
            [ type_ "radio"
            , class "skjemaelement__input radioknapp"
            , checked options.checked
            , onClick options.msg
            ]
            []

        --- TODO: htmlFor
        , label [ class "skjemaelement__label", class "skjemaelement--checkbox-label", onClick options.msg ] [ text options.label ]
        ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []


isChecked : Radio msg -> Bool
isChecked (Radio options) =
    options.checked


tekst : Radio msg -> String
tekst (Radio options) =
    options.label


checkedRadioToString : List (Radio msg) -> String
checkedRadioToString radioknapper =
    radioknapper
        |> List.find (\radio_ -> isChecked radio_)
        |> Maybe.map tekst
        |> Maybe.withDefault ""
