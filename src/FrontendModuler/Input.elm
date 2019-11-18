module FrontendModuler.Input exposing
    ( Enabled(..)
    , Input
    , InputOptions
    , input
    , inputWithPlaceholder
    , toHtml
    , withClass
    , withEnabled
    , withFeilmelding
    , withId
    , withMaybeFeilmelding
    , withOnBlur
    , withOnEnter
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Json.Decode


type Input msg
    = Input (Options msg)


type alias Options msg =
    { msg : String -> msg
    , label : String
    , innhold : String
    , feilmelding : Maybe String
    , classes : List String
    , onEnter : Maybe msg
    , onBlur : Maybe msg
    , id : Maybe String
    , enabled : Enabled
    , placeholder : Maybe String
    }


type alias InputOptions msg =
    { msg : String -> msg
    , label : String
    }


type alias InputOptionsWithPlaceholder msg =
    { msg : String -> msg
    , label : String
    , placeholder : String
    }


input : InputOptions msg -> String -> Input msg
input { msg, label } innhold =
    Input
        { msg = msg
        , label = label
        , innhold = innhold
        , feilmelding = Nothing
        , classes = []
        , onEnter = Nothing
        , onBlur = Nothing
        , id = Nothing
        , enabled = Enabled
        , placeholder = Nothing
        }


inputWithPlaceholder : InputOptionsWithPlaceholder msg -> String -> Input msg
inputWithPlaceholder { msg, label, placeholder } innhold =
    Input
        { msg = msg
        , label = label
        , innhold = innhold
        , feilmelding = Nothing
        , classes = []
        , onEnter = Nothing
        , onBlur = Nothing
        , id = Nothing
        , enabled = Enabled
        , placeholder = Just placeholder
        }


type Enabled
    = Enabled
    | Disabled


withFeilmelding : String -> Input msg -> Input msg
withFeilmelding feilmelding (Input options) =
    Input { options | feilmelding = Just feilmelding }


withMaybeFeilmelding : Maybe String -> Input msg -> Input msg
withMaybeFeilmelding feilmelding (Input options) =
    Input { options | feilmelding = feilmelding }


withEnabled : Enabled -> Input msg -> Input msg
withEnabled enabled (Input options) =
    Input { options | enabled = enabled }


withClass : String -> Input msg -> Input msg
withClass class (Input options) =
    Input { options | classes = class :: options.classes }


withOnEnter : msg -> Input msg -> Input msg
withOnEnter msg (Input info) =
    Input { info | onEnter = Just msg }


withOnBlur : msg -> Input msg -> Input msg
withOnBlur msg (Input info) =
    Input { info | onBlur = Just msg }


withId : String -> Input msg -> Input msg
withId id (Input info) =
    Input { info | id = Just id }



--- HTML ---


onEnter : msg -> Html.Attribute msg
onEnter msg =
    Html.Events.preventDefaultOn
        "keydown"
        (Json.Decode.andThen (decodeEnter msg) Html.Events.keyCode)


decodeEnter : msg -> Int -> Json.Decode.Decoder ( msg, Bool )
decodeEnter msg i =
    if i == 13 then
        Json.Decode.succeed ( msg, True )

    else
        Json.Decode.fail ""


toHtml : Input msg -> Html msg
toHtml (Input options) =
    div [ class "skjemaelement" ]
        [ label
            --- TODO: htmlFor={inputId}
            []
            [ span [ class "skjemaelement__label" ] [ text options.label ]
            , Html.input
                [ type_ "text"
                , value options.innhold
                , classList
                    [ ( "skjemaelement__input", True )
                    , ( "input--fullbredde", True )
                    , ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing )
                    ]
                , optionClasses options.classes
                , options.placeholder
                    |> Maybe.map placeholder
                    |> Maybe.withDefault noAttribute
                , onInput options.msg
                , options.id
                    |> Maybe.map id
                    |> Maybe.withDefault noAttribute
                , options.onEnter
                    |> Maybe.map onEnter
                    |> Maybe.withDefault noAttribute
                , options.onBlur
                    |> Maybe.map onBlur
                    |> Maybe.withDefault noAttribute
                , disabled (options.enabled == Disabled)
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


optionClasses : List String -> Html.Attribute msg
optionClasses classes =
    classes
        |> List.map (\class_ -> ( class_, True ))
        |> Html.Attributes.classList


noAttribute : Html.Attribute msg
noAttribute =
    classList []
