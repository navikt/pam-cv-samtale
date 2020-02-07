module FrontendModuler.Radio exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias RadioOption =
    { label : String
    , value : String
    }


type alias RadioOptionWithChecked =
    { label : String
    , value : String
    , checked : Bool
    }


type Radio msg
    = Radio
        { options : List RadioOption
        , msg : msg
        , checked : RadioOption
        }


radio : List RadioOption -> msg -> RadioOption -> Radio msg
radio options msg checked =
    Radio
        { options = options
        , msg = msg
        , checked = checked
        }


getRadioOptionsWithChecked : RadioOption -> List RadioOption -> List RadioOptionWithChecked
getRadioOptionsWithChecked checkedRadioOption radioOptions =
    List.map
        (\radioOption ->
            if radioOption == checkedRadioOption then
                RadioOptionWithChecked radioOption.label radioOption.value True

            else
                RadioOptionWithChecked radioOption.label radioOption.value False
        )
        radioOptions


optionToHtml : RadioOptionWithChecked -> Html msg
optionToHtml radioOption =
    div
        [ class "skjemaelement skjemaelement--horisontal"
        ]
        [ input
            [ type_ "checkbox"
            , class "skjemaelement__input checkboks"
            , checked radioOption.checked
            ]
            []
        , label [ class "skjemaelement__label skjemaelement--checkbox-label" ] [ text radioOption.label ]
        ]


toHtml : Radio msg -> Html msg
toHtml (Radio radioObject) =
    div
        []
        (List.map optionToHtml (getRadioOptionsWithChecked radioObject.checked radioObject.options))
