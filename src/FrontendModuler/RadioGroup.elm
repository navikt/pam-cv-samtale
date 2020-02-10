module FrontendModuler.RadioGroup exposing (..)

import FrontendModuler.Radio as Radio
import Html exposing (Html, div)


type alias RadioOption =
    { value : String, label : String }


type RadioGroup msg
    = RadioGroup
        { options : List RadioOption
        , msg : msg
        , checked : Maybe RadioOption
        }


radioGroup : List RadioOption -> msg -> Maybe RadioOption -> RadioGroup msg
radioGroup options msg checked =
    RadioGroup
        { options = options
        , msg = msg
        , checked = checked
        }


renderRadioOption : RadioGroup msg -> RadioOption -> Html msg
renderRadioOption msg option =
    Radio.toHtml (Radio.radio option.label option.value msg True)


toHtml : RadioGroup msg -> Html msg
toHtml (RadioGroup group) =
    div
        []
        [ List.map renderRadioOption group.options ]
