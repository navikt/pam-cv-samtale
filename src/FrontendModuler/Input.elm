module FrontendModuler.Input exposing
    ( Enabled(..)
    , Input
    , InputOptions
    , innhold
    , input
    , toHtml
    , withClass
    , withEnabled
    , withErObligatorisk
    , withFeilmelding
    , withId
    , withMaybeFeilmelding
    , withOnBlur
    , withOnEnter
    , withPlaceholder
    , withWrapperClass
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
    , wrapperClasses : List String
    , onEnter : Maybe msg
    , onBlur : Maybe msg
    , id : Maybe String
    , enabled : Enabled
    , placeholder : Maybe String
    , ariaLabelledby : Maybe msg
    , obligatorisk : Bool
    }


type alias InputOptions msg =
    { msg : String -> msg
    , label : String
    }


input : InputOptions msg -> String -> Input msg
input { msg, label } innhold_ =
    Input
        { msg = msg
        , label = Label label
        , innhold = innhold_
        , feilmelding = Nothing
        , classes = []
        , wrapperClasses = []
        , onEnter = Nothing
        , onBlur = Nothing
        , id = Nothing
        , enabled = Enabled
        , placeholder = Nothing
        , ariaLabelledby = Nothing
        , obligatorisk = False
        }


type Enabled
    = Enabled
    | Disabled


withFeilmelding : String -> Input msg -> Input msg
withFeilmelding feilmelding (Input options) =
    Input { options | feilmelding = Just feilmelding }


withErObligatorisk : Input msg -> Input msg
withErObligatorisk (Input options) =
    Input { options | obligatorisk = True }


withMaybeFeilmelding : Maybe String -> Input msg -> Input msg
withMaybeFeilmelding feilmelding (Input options) =
    Input { options | feilmelding = feilmelding }


withEnabled : Enabled -> Input msg -> Input msg
withEnabled enabled (Input options) =
    Input { options | enabled = enabled }


withClass : String -> Input msg -> Input msg
withClass class (Input options) =
    Input { options | classes = class :: options.classes }


withWrapperClass : String -> Input msg -> Input msg
withWrapperClass class (Input options) =
    Input { options | wrapperClasses = class :: options.wrapperClasses }


withOnEnter : msg -> Input msg -> Input msg
withOnEnter msg (Input info) =
    Input { info | onEnter = Just msg }


withOnBlur : msg -> Input msg -> Input msg
withOnBlur msg (Input info) =
    Input { info | onBlur = Just msg }


withId : String -> Input msg -> Input msg
withId id (Input info) =
    Input { info | id = Just id }


withPlaceholder : String -> Input msg -> Input msg
withPlaceholder placeholder (Input info) =
    Input { info | placeholder = Just placeholder }



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
    div [ class "skjemaelement", optionClasses options.wrapperClasses ]
        (case options.label of
            Label label_ ->
                [ label []
                    [ span [ class "skjemaelement__label" ]
                        (if options.obligatorisk then
                            [ text label_
                            , span [ class "skjemaelement__måFyllesUt" ] [ text " - må fylles ut" ]
                            ]

                         else
                            [ text label_ ]
                        )
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


innhold : Input msg -> String
innhold (Input options) =
    options.innhold
