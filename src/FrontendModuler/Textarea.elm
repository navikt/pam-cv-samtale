module FrontendModuler.Textarea exposing
    ( Textarea
    , TextareaOptions
    , textarea
    , toHtml
    ,  withFeilmelding
       --    , withMaxLength

    , withMaybeFeilmelding
    , withTextAreaClass
    )

import Html exposing (Html, div, label, p, text)
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
    , maxLength : Maybe Int
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
        , maxLength = Nothing
        }


withTextAreaClass : String -> Textarea msg -> Textarea msg
withTextAreaClass textAreaClass (Textarea options) =
    Textarea { options | textAreaClass = textAreaClass }


withFeilmelding : String -> Textarea msg -> Textarea msg
withFeilmelding feilmelding (Textarea options) =
    Textarea { options | feilmelding = Just feilmelding }


withMaybeFeilmelding : Maybe String -> Textarea msg -> Textarea msg
withMaybeFeilmelding maybeFeilmelding (Textarea options) =
    Textarea { options | feilmelding = maybeFeilmelding }



--withMaxLength : Int -> Textarea msg -> Textarea msg
--withMaxLength maxLength (Textarea options) =
--    Textarea { options | maxLength = Just maxLength }


toHtml : Textarea msg -> Html msg
toHtml (Textarea options) =
    div [ class "skjemaelement textarea__container" ]
        --- TODO: htmlFor={inputId}
        [ label [ class "skjemaelement__label" ] [ text options.label ]
        , div [ class "textarea--medMeta__wrapper" ]
            [ Html.textarea
                [ onInput options.msg
                , classList
                    [ ( "skjemaelement__input textarea--medMeta", True )
                    , ( options.textAreaClass, True )
                    , ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing )
                    , ( "overflow-auto-textbox", True ) -- Klassen er for at man skal kunne legge til overflow auto, som ikke er med i designsystemet
                    ]
                , value options.innhold
                ]
                []
            , case options.maxLength of
                Just maxLength ->
                    p [ class "textarea--medMeta__teller" ]
                        [ if String.length options.innhold <= maxLength then
                            let
                                tallTekst =
                                    (maxLength - String.length options.innhold)
                                        |> String.fromInt
                            in
                            text ("Du har " ++ tallTekst ++ " tegn igjen")

                          else
                            let
                                tallTekst =
                                    (String.length options.innhold - maxLength)
                                        |> String.fromInt
                            in
                            text ("Du har " ++ tallTekst ++ " tegn for mye")
                        ]

                Nothing ->
                    text ""
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
