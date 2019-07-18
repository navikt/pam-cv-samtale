module FrontendModuler.Input exposing
    ( Input
    , InputOptions
    , input
    , toHtml
    , withFeilmelding
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)


type Input msg
    = Input (Options msg)


type alias Options msg =
    { msg : String -> msg
    , label : String
    , innhold : String
    , feilmelding : Maybe String
    }


type alias InputOptions msg =
    { msg : String -> msg
    , label : String
    }


input : InputOptions msg -> String -> Input msg
input { msg, label } innhold =
    Input
        { msg = msg
        , label = label
        , innhold = innhold
        , feilmelding = Nothing
        }


withFeilmelding : String -> Input msg -> Input msg
withFeilmelding feilmelding (Input options) =
    Input { options | feilmelding = Just feilmelding }


toHtml : Input msg -> Html msg
toHtml (Input options) =
    div [ class "skjemaelement" ]
        [ label
            --- TODO: htmlFor={inputId}
            [ class "skjemaelement__label" ]
            [ text options.label ]
        , Html.input
            [ type_ "text"
            , value options.innhold
            , classList [ ( "skjemaelement__input", True ), ( "input--fullbredde", True ), ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing ) ]
            , onInput options.msg
            ]
            []
        , case options.feilmelding of
            Just feilmelding ->
                div [ role "alert", ariaLive "assertive" ]
                    [ div [ class "skjemaelement__feilmelding" ]
                        [ text feilmelding ]
                    ]

            Nothing ->
                text ""
        ]
