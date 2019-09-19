module FrontendModuler.Select exposing
    ( Select
    , select
    , toHtml
    , withClass
    , withSelected
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type Select msg
    = Select
        { label : String
        , msg : String -> msg
        , listOfOptions : List ( String, String )
        , selectedValue : Maybe String
        , class : Maybe String
        }


select : String -> (String -> msg) -> List ( String, String ) -> Select msg
select label msg listOfOptions =
    Select
        { label = label
        , msg = msg
        , listOfOptions = listOfOptions
        , selectedValue = Nothing
        , class = Nothing
        }


withSelected : String -> Select msg -> Select msg
withSelected selectedValue (Select options) =
    Select { options | selectedValue = Just selectedValue }


withClass : String -> Select msg -> Select msg
withClass class (Select options) =
    Select { options | class = Just class }


toHtml : Select msg -> Html msg
toHtml (Select options) =
    div [ class "skjemaelement" ]
        [ label [ class "skjemaelement__label" ] [ text options.label ]
        , div [ class "selectContainer input--fullbredde" ]
            [ List.map (optionToHtml options.selectedValue) options.listOfOptions
                |> Html.select
                    [ onInput options.msg
                    , class "skjemaelement__input"
                    , options.class
                        |> Maybe.map class
                        |> Maybe.withDefault noAttribute
                    ]
            ]
        ]


optionToHtml : Maybe String -> ( String, String ) -> Html msg
optionToHtml maybeSelectedValue ( valueString, tekst ) =
    option
        [ value valueString
        , maybeSelectedValue
            |> Maybe.map (\selectedValue -> selectedValue == valueString)
            |> Maybe.withDefault False
            |> selected
        ]
        [ text tekst ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []
