module FrontendModuler.Select exposing
    ( Select
    , select
    , toHtml
    , withClass
    , withLabelId
    , withMaybeFeilmelding
    , withMaybeSelected
    , withSelected
    )

import FrontendModuler.Feilmelding exposing (htmlFeilmelding)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabelledby)
import Html.Events exposing (onInput)


type Label
    = Label String
    | LabelId String


type Select msg
    = Select
        { label : Label
        , msg : String -> msg
        , listOfOptions : List ( String, String )
        , selectedValue : Maybe String
        , feilmelding : Maybe String
        , class : Maybe String
        }


select : String -> (String -> msg) -> List ( String, String ) -> Select msg
select label msg listOfOptions =
    Select
        { label = Label label
        , msg = msg
        , listOfOptions = listOfOptions
        , selectedValue = Nothing
        , feilmelding = Nothing
        , class = Nothing
        }


withLabelId : String -> Select msg -> Select msg
withLabelId id (Select options) =
    Select { options | label = LabelId id }


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
        (case options.label of
            Label label_ ->
                [ label []
                    [ span [ class "skjemaelement__label" ] [ text label_ ]
                    , htmlSelect (Select options) Nothing
                    ]
                , htmlFeilmelding options.feilmelding
                ]

            LabelId id_ ->
                [ htmlSelect (Select options) (Just id_)
                , htmlFeilmelding options.feilmelding
                ]
        )


htmlSelect : Select msg -> Maybe String -> Html msg
htmlSelect (Select options) labelId =
    div [ class "selectContainer input--s" ]
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
                , labelId
                    |> Maybe.map ariaLabelledby
                    |> Maybe.withDefault noAttribute
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
