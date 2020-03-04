module FrontendModuler.Radio exposing
    ( Radio
    , checkedRadioToString
    , radio
    , toHtml
    , withClass
    , withId
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra as List


type Radio msg
    = Radio
        { label : String
        , msg : msg
        , name : String
        , class : Maybe String
        , id : Maybe String
        , checked : Bool
        }


radio : { gruppeNavn : String, label : String, onSelected : msg, selected : Bool } -> Radio msg
radio { gruppeNavn, label, onSelected, selected } =
    Radio
        { label = label
        , msg = onSelected
        , name = gruppeNavn
        , class = Nothing
        , id = Nothing
        , checked = selected
        }


withClass : String -> Radio msg -> Radio msg
withClass class (Radio options) =
    Radio { options | class = Just class }


withId : String -> Radio msg -> Radio msg
withId id (Radio options) =
    Radio { options | id = Just id }


toHtml : Radio msg -> Html msg
toHtml (Radio options) =
    div
        [ class "skjemaelement skjemaelement--horisontal"
        , options.class
            |> Maybe.map class
            |> Maybe.withDefault noAttribute
        ]
        [ label
            []
            [ input
                [ type_ "radio"
                , class "skjemaelement__input radioknapp"
                , name options.name
                , checked options.checked
                , onClick options.msg
                , options.id
                    |> Maybe.map id
                    |> Maybe.withDefault noAttribute
                ]
                []
            , span [ class "skjemaelement__label skjemaelement--checkbox-label" ] [ text options.label ]
            ]
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
