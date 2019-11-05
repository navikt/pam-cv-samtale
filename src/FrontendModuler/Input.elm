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
    , withLabelId
    , withMaybeFeilmelding
    , withOnBlur
    , withOnEnter
    )

import FrontendModuler.Feilmelding as Feilmelding
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLabelledby)
import Html.Events exposing (..)
import Json.Decode


type Input msg
    = Input (Options msg)


type Label
    = Label String
    | LabelId String


type alias Options msg =
    { msg : String -> msg
    , label : Label
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
        , label = Label label
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


withLabelId : String -> Input msg -> Input msg
withLabelId id (Input options) =
    Input { options | label = LabelId id }


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
            Label label_ ->
                [ label []
                    [ span [ class "skjemaelement__label" ] [ text label_ ]
                    , htmlInput (Input options) Nothing
                    ]
                , Feilmelding.htmlFeilmelding options.feilmelding
                ]

            LabelId id_ ->
                [ htmlInput (Input options) (Just id_)
                , Feilmelding.htmlFeilmelding options.feilmelding
                ]
        )


htmlInput : Input msg -> Maybe String -> Html msg
htmlInput (Input options) labelId =
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
        , labelId
            |> Maybe.map ariaLabelledby
            |> Maybe.withDefault noAttribute
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
