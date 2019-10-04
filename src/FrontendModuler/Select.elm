module FrontendModuler.Select exposing
    ( Select
    , select
    , toHtml
    , withClass
    , withMaybeFeilmelding
    , withMaybeSelected
    , withSelected
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (onInput)


type Select msg
    = Select
        { label : String
        , msg : String -> msg
        , listOfOptions : List ( String, String )
        , selectedValue : Maybe String
        , feilmelding : Maybe String
        , class : Maybe String
        }


select : String -> (String -> msg) -> List ( String, String ) -> Select msg
select label msg listOfOptions =
    Select
        { label = label
        , msg = msg
        , listOfOptions = listOfOptions
        , selectedValue = Nothing
        , feilmelding = Nothing
        , class = Nothing
        }


withSelected : String -> Select msg -> Select msg
withSelected selectedValue (Select options) =
    Select { options | selectedValue = Just selectedValue }


withMaybeSelected : Maybe String -> Select msg -> Select msg
withMaybeSelected selectedValue (Select options) =
    Select { options | selectedValue = selectedValue }


withMaybeFeilmelding : Maybe String -> Select msg -> Select msg
withMaybeFeilmelding feilmelding (Select options) =
    Select { options | feilmelding = feilmelding }


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
                    , classList
                        [ ( "skjemaelement__input", True )
                        , ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing )
                        ]
                    , options.class
                        |> Maybe.map class
                        |> Maybe.withDefault noAttribute
                    ]
            ]
        , case options.feilmelding of
            Just feilemlding ->
                div [ role "alert", ariaLive "assertive" ] [ div [ class "skjemaelement__feilmelding" ] [ text feilemlding ] ]

            Nothing ->
                text ""
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
