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
        , value : String
        , msg : msg
        , checked : Bool
        , class : Maybe String
        , id : Maybe String
        }


radio : String -> String -> msg -> Bool -> Radio msg
radio label value msg checked =
    Radio
        { label = label
        , value = value
        , msg = msg
        , checked = checked
        , class = Nothing
        , id = Nothing
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
        [ input
            (buildInputAttributes options.checked options.msg options.id)
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


buildInputAttributes : Bool -> msg -> Maybe String -> List (Attribute msg)
buildInputAttributes checkedStatus onClickMsg inputId =
    case inputId of
        Nothing ->
            [ type_ "checkbox"
            , class "skjemaelement__input radioknapp"
            , checked checkedStatus
            , onClick onClickMsg
            ]

        Just verifiedInputId ->
            [ type_ "checkbox"
            , class "skjemaelement__input radioknapp"
            , checked checkedStatus
            , onClick onClickMsg
            , id verifiedInputId
            ]


checkedRadioToString : List (Radio msg) -> String
checkedRadioToString radioknapper =
    radioknapper
        |> List.find (\radio_ -> isChecked radio_)
        |> Maybe.map tekst
        |> Maybe.withDefault ""
