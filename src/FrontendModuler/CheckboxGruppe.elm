module FrontendModuler.CheckboxGruppe exposing (CheckboxGruppe, checkboxGruppe, toHtml)

import FrontendModuler.Checkbox as Checkbox exposing (Checkbox)
import Html exposing (..)
import Html.Attributes exposing (..)


type CheckboxGruppe msg
    = CheckboxGruppe (Info msg)


type alias Info msg =
    { legend : String
    , fokusId : Maybe String
    , checkboxer : List (Checkbox msg)
    }


type alias CheckboxGruppeInfo msg =
    { legend : String
    , checkboxer : List (Checkbox msg)
    }


checkboxGruppe : CheckboxGruppeInfo msg -> CheckboxGruppe msg
checkboxGruppe { legend, checkboxer } =
    CheckboxGruppe
        { legend = legend
        , fokusId = Nothing
        , checkboxer = checkboxer
        }


toHtml : CheckboxGruppe msg -> Html msg
toHtml (CheckboxGruppe options) =
    fieldset
        [ class "radio-checkbox-fieldset radio-checkbox-gruppe-wrapper"
        ]
        [ legend [ class "skjemaelement__label" ]
            [ text options.legend ]
        , div
            []
            (List.map
                Checkbox.toHtml
                options.checkboxer
            )
        ]
