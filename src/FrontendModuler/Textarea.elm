module FrontendModuler.Textarea exposing
    ( Textarea
    , TextareaOptions
    , innhold
    , textarea
    ,  toHtml
       --    , withMaxLength

    , withFeilmelding
    , withId
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
    , id : Maybe String
    }


type alias TextareaOptions msg =
    { msg : String -> msg
    , label : String
    }


textarea : TextareaOptions msg -> String -> Textarea msg
textarea { msg, label } innhold_ =
    Textarea
        { msg = msg
        , label = label
        , innhold = innhold_
        , textAreaClass = ""
        , feilmelding = Nothing
        , maxLength = Nothing
        , id = Nothing
        }


withTextAreaClass : String -> Textarea msg -> Textarea msg
withTextAreaClass textAreaClass (Textarea options) =
    Textarea { options | textAreaClass = textAreaClass }


withFeilmelding : Maybe String -> Textarea msg -> Textarea msg
withFeilmelding maybeFeilmelding (Textarea options) =
    Textarea { options | feilmelding = maybeFeilmelding }


withId : String -> Textarea msg -> Textarea msg
withId id (Textarea options) =
    Textarea { options | id = Just id }



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
                , options.id
                    |> Maybe.map id
                    |> Maybe.withDefault noAttribute
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
                        [ p [ class "typo-feilmelding" ] [ text feilmelding ] ]
                    ]

            Nothing ->
                text ""
        ]


noAttribute : Html.Attribute msg
noAttribute =
    classList []


innhold : Textarea msg -> String
innhold (Textarea options) =
    options.innhold
