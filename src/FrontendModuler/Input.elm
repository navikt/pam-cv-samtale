module FrontendModuler.Input exposing
    ( Class(..)
    , Input
    , InputOptions
    , input
    , toHtml
    , withClass
    , withFeilmelding
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)


type Input msg
    = Input (Options msg)


type Class
    = År


type alias Options msg =
    { msg : String -> msg
    , label : String
    , innhold : String
    , feilmelding : Maybe String
    , class : Maybe Class
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
        , class = Nothing
        }


withFeilmelding : String -> Input msg -> Input msg
withFeilmelding feilmelding (Input options) =
    Input { options | feilmelding = Just feilmelding }


withClass : Class -> Input msg -> Input msg
withClass class (Input options) =
    Input { options | class = Just class }


toHtml : Input msg -> Html msg
toHtml (Input options) =
    case options.class of
        Nothing ->
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

        Just År ->
            div [ class "skjemaelement" ]
                [ label
                    --- TODO: htmlFor={inputId}
                    [ class "skjemaelement__label" ]
                    [ text options.label ]
                , Html.input
                    [ type_ "text"
                    , value options.innhold
                    , classList
                        [ ( "skjemaelement__input", True )
                        , ( "input--fullbredde", True )
                        , ( "skjemaelement__input--harFeil"
                          , options.feilmelding /= Nothing
                          )
                        , ( "år", True )
                        ]
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
