module FrontendModuler.Checkbox exposing
    ( Checkbox
    , checkbox
    , toHtml
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Checkbox msg
    = Checkbox
        { label : String
        , msg : msg
        , checked : Bool
        }


checkbox : String -> msg -> Bool -> Checkbox msg
checkbox label msg checkboxChecked =
    Checkbox
        { label = label
        , msg = msg
        , checked = checkboxChecked
        }


toHtml : Checkbox msg -> Html msg
toHtml (Checkbox options) =
    div [ class "skjemaelement skjemaelement--horisontal" ]
        [ input
            [ type_ "checkbox"
            , class "skjemaelement__input checkboks"
            , checked options.checked
            , onClick options.msg
            ]
            []

        --- TODO: htmlFor
        , label [ class "skjemaelement__label", class "skjemaelement--checkbox-label", onClick options.msg ] [ text options.label ]
        ]
