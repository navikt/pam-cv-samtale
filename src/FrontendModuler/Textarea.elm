module FrontendModuler.Textarea exposing
    ( Textarea
    , TextareaOptions
    , textarea
    , toHtml
    , withFeilmelding
    , withTextAreaClass
    )

import Html exposing (Html, div, label, text)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLive, role)
import Html.Events exposing (onInput)


type Textarea msg
    = Textarea (Options msg)


type alias Options msg =
    { msg : String -> msg
    , label : String
    , innhold : String
    , textAreaClass : String
    , feilmelding : Maybe String
    }


type alias TextareaOptions msg =
    { msg : String -> msg
    , label : String
    }


textarea : TextareaOptions msg -> String -> Textarea msg
textarea { msg, label } innhold =
    Textarea
        { msg = msg
        , label = label
        , innhold = innhold
        , textAreaClass = ""
        , feilmelding = Nothing
        }


withTextAreaClass : String -> Textarea msg -> Textarea msg
withTextAreaClass textAreaClass (Textarea options) =
    Textarea { options | textAreaClass = textAreaClass }


withFeilmelding : String -> Textarea msg -> Textarea msg
withFeilmelding feilmelding (Textarea options) =
    Textarea { options | feilmelding = Just feilmelding }


toHtml : Textarea msg -> Html msg
toHtml (Textarea options) =
    div [ class "skjemaelement textarea__container" ]
        --- TODO: htmlFor={inputId}
        [ label [ class "skjemaelement__label" ] [ text options.label ]
        , div [ class "textarea--medMeta__wrapper" ]
            [ Html.textarea
                [ onInput options.msg
                , classList [ ( "skjemaelement__input textarea--medMeta", True ), ( options.textAreaClass, True ), ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing ) ]
                , value options.innhold
                ]
                []
            ]
        , case options.feilmelding of
            Just feilmelding ->
                div [ role "alert", ariaLive "assertive" ]
                    [ div [ class "skjemaelement__feilmelding" ]
                        [ text feilmelding ]
                    ]

            Nothing ->
                text ""
        ]
