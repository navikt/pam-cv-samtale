module FrontendModuler.Input exposing
    ( Enabled(..)
    , Input
    , InputOptions
    , input
    , toHtml
    , withClass
    , withEnabled
    , withFeilmelding
    , withId
    , withMaybeFeilmelding
    , withOnBlur
    , withOnEnter
    , withoutLabel
    )

import FrontendModuler.Feilmelding exposing (htmlFeilmelding)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode


type Input msg
    = Input (Options msg)


type alias Options msg =
    { msg : String -> msg
    , label : Maybe String
    , innhold : String
    , feilmelding : Maybe String
    , classes : List String
    , onEnter : Maybe msg
    , onBlur : Maybe msg
    , id : Maybe String
    , enabled : Enabled
    , ariaLabelledby : Maybe msg
    }


type alias InputOptions msg =
    { msg : String -> msg
    , label : String
    }


input : InputOptions msg -> String -> Input msg
input { msg, label } innhold =
    Input
        { msg = msg
        , label = Just label
        , innhold = innhold
        , feilmelding = Nothing
        , classes = []
        , onEnter = Nothing
        , onBlur = Nothing
        , id = Nothing
        , enabled = Enabled
        , ariaLabelledby = Nothing
        }


type Enabled
    = Enabled
    | Disabled


withoutLabel : Input msg -> Input msg
withoutLabel (Input options) =
    Input { options | label = Nothing }


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
        (case options.label of
            Just label_ ->
                [ label []
                    [ span [ class "skjemaelement__label" ] [ text label_ ]
                    , htmlInput (Input options)
                    ]
                , htmlFeilmelding options.feilmelding
                ]

            Nothing ->
                [ htmlInput (Input options)
                , htmlFeilmelding options.feilmelding
                ]
        )


htmlInput : Input msg -> Html msg
htmlInput (Input options) =
    Html.input
        [ type_ "text"
        , value options.innhold
        , classList
            [ ( "skjemaelement__input", True )
            , ( "input--fullbredde", True )
            , ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing )
            ]
        , optionClasses options.classes
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


optionClasses : List String -> Html.Attribute msg
optionClasses classes =
    classes
        |> List.map (\class_ -> ( class_, True ))
        |> Html.Attributes.classList


noAttribute : Html.Attribute msg
noAttribute =
    classList []
