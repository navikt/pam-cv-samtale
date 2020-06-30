module FrontendModuler.Typeahead exposing
    ( Operation(..)
    , Suggestion
    , Typeahead
    , TypeaheadOptions
    , innhold
    , map
    , scrollActiveSuggestionIntoView
    , toHtml
    , typeahead
    , withErObligatorisk
    , withErrorMelding
    , withFeilmelding
    , withOnBlur
    , withOnFocus
    , withPrøvIgjenKnapp
    , withSuggestions
    )

import Browser.Dom as Dom
import FrontendModuler.Alertstripe as Alertstripe
import FrontendModuler.CoronaInfobox as CoronaInfobox
import FrontendModuler.Feilmelding exposing (htmlFeilmelding)
import FrontendModuler.Knapp as Knapp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Json.Decode.Pipeline
import List.Extra as List
import Svg exposing (g, path, svg)
import Svg.Attributes exposing (d, fill, viewBox)
import Task exposing (Task)


type Typeahead msg
    = Typeahead (TypeaheadInfo msg)


type alias TypeaheadInfo msg =
    { label : String
    , onInput : String -> msg
    , onTypeaheadChange : Operation -> msg
    , innhold : String
    , suggestions : List (Suggestion msg)
    , inputId : String
    , feilmelding : Maybe String
    , errorMelding : Maybe String
    , prøvIgjenMsg : Maybe msg
    , onFocus : Maybe msg
    , onBlur : Maybe msg
    , obligatorisk : Bool
    }


type Operation
    = ArrowUp
    | ArrowDown
    | Enter
    | MouseLeaveSuggestions


type alias TypeaheadOptions msg =
    { onInput : String -> msg
    , onTypeaheadChange : Operation -> msg
    , label : String
    , inputId : String
    }


typeahead : TypeaheadOptions msg -> String -> Typeahead msg
typeahead options innhold_ =
    Typeahead
        { label = options.label
        , onInput = options.onInput
        , onTypeaheadChange = options.onTypeaheadChange
        , innhold = innhold_
        , suggestions = []
        , inputId = options.inputId
        , feilmelding = Nothing
        , errorMelding = Nothing
        , prøvIgjenMsg = Nothing
        , onFocus = Nothing
        , onBlur = Nothing
        , obligatorisk = False
        }


type alias Suggestion msg =
    { innhold : String
    , onActive : { x : Float, y : Float } -> msg
    , onClick : msg
    , active : Bool
    }


withSuggestions : List (Suggestion msg) -> Typeahead msg -> Typeahead msg
withSuggestions suggestions (Typeahead options) =
    Typeahead { options | suggestions = suggestions }


withFeilmelding : Maybe String -> Typeahead msg -> Typeahead msg
withFeilmelding feilmelding (Typeahead options) =
    Typeahead { options | feilmelding = feilmelding }


withErrorMelding : Maybe String -> Typeahead msg -> Typeahead msg
withErrorMelding errorMelding (Typeahead options) =
    Typeahead { options | errorMelding = errorMelding }


withPrøvIgjenKnapp : Maybe msg -> Typeahead msg -> Typeahead msg
withPrøvIgjenKnapp prøvIgjenMsg (Typeahead options) =
    Typeahead { options | prøvIgjenMsg = prøvIgjenMsg }


withOnFocus : msg -> Typeahead msg -> Typeahead msg
withOnFocus onFocus (Typeahead options) =
    Typeahead { options | onFocus = Just onFocus }


withOnBlur : msg -> Typeahead msg -> Typeahead msg
withOnBlur onBlur (Typeahead options) =
    Typeahead { options | onBlur = Just onBlur }


withErObligatorisk : Typeahead msg -> Typeahead msg
withErObligatorisk (Typeahead options) =
    Typeahead { options | obligatorisk = True }


onKeyUp : (Operation -> msg) -> Html.Attribute msg
onKeyUp onTypeaheadChange =
    Html.Events.preventDefaultOn
        "keydown"
        (Json.Decode.andThen (typeaheadKeys onTypeaheadChange) Html.Events.keyCode)


typeaheadKeys : (Operation -> msg) -> Int -> Json.Decode.Decoder ( msg, Bool )
typeaheadKeys onTypeaheadChange i =
    if i == 40 then
        Json.Decode.succeed ( onTypeaheadChange ArrowDown, True )

    else if i == 38 then
        Json.Decode.succeed ( onTypeaheadChange ArrowUp, True )

    else if i == 13 then
        Json.Decode.succeed ( onTypeaheadChange Enter, True )

    else
        Json.Decode.fail ""


toHtml : Typeahead msg -> Html msg
toHtml (Typeahead options) =
    div [ class "typeahead-wrapper" ]
        [ div [ class "typeahead" ]
            -- todo: RYDD_OPP_KORONA
            [ case options.label of
                "Kompetanser" ->
                    div []
                        [ CoronaInfobox.coronaInfobox "Har du viktig kompetanse?"
                            "Har du kompetanse som samfunnet trenger akkurat nå? For eksempel gårdsdrift eller helsefag? Legg inn kompetansene. Også annen type kompetanse kan være aktuelt."
                            |> CoronaInfobox.toHtml
                        ]

                _ ->
                    text ""
            , label
                [ class "skjemaelement__label", for options.inputId ]
                (if options.obligatorisk then
                    [ text options.label
                    , span [ class "skjemaelement__måFyllesUt" ] [ text " - må fylles ut" ]
                    ]

                 else
                    [ text options.label ]
                )
            , input
                [ onInput options.onInput
                , value options.innhold
                , class "skjemaelement__input input--fullbredde"
                , classList [ ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing ) ]
                , onKeyUp options.onTypeaheadChange
                , type_ "text"
                , id options.inputId
                , autocomplete False
                , role "combobox"
                , ariaAutocompleteList
                , ariaOwns (suggestionsId options.inputId)
                , ariaControls (suggestionsId options.inputId)
                , activeDescendant options.inputId options.suggestions
                , options.onFocus
                    |> Maybe.map onFocus
                    |> Maybe.withDefault noAttribute
                , options.onBlur
                    |> Maybe.map onBlur
                    |> Maybe.withDefault noAttribute
                ]
                []
            , case options.errorMelding of
                Just errorMelding ->
                    div
                        [ class "suggestion-list typeahead-error"
                        , role "alert"
                        , ariaLive "assertive"
                        , tabindex -1
                        , options.onFocus
                            |> Maybe.map onFocus
                            |> Maybe.withDefault noAttribute
                        , options.onBlur
                            |> Maybe.map onBlur
                            |> Maybe.withDefault noAttribute
                        ]
                        [ p []
                            [ text errorMelding ]
                        , case options.prøvIgjenMsg of
                            Just prøvIgjenMsg ->
                                Knapp.knapp prøvIgjenMsg "Prøv igjen"
                                    |> Knapp.withAttribute (options.onBlur |> Maybe.map onBlur |> Maybe.withDefault noAttribute)
                                    |> Knapp.withAttribute (options.onFocus |> Maybe.map onFocus |> Maybe.withDefault noAttribute)
                                    |> Knapp.toHtml

                            Nothing ->
                                text ""
                        ]

                Nothing ->
                    if List.isEmpty options.suggestions then
                        text ""

                    else
                        options.suggestions
                            |> List.map (viewSuggestion options.inputId)
                            |> ul
                                [ onMouseLeave (options.onTypeaheadChange MouseLeaveSuggestions)
                                , class "suggestion-list"
                                , id (suggestionsId options.inputId)
                                , ariaExpanded "true"
                                , role "listbox"
                                , tabindex -1
                                , options.onFocus
                                    |> Maybe.map onFocus
                                    |> Maybe.withDefault noAttribute
                                ]
            ]
        , case options.errorMelding of
            Nothing ->
                htmlFeilmelding options.feilmelding

            _ ->
                text ""
        ]


activeDescendant : String -> List (Suggestion msg) -> Html.Attribute msg
activeDescendant inputFeltId suggestions =
    suggestions
        |> List.find .active
        |> Maybe.map (suggestionId inputFeltId)
        |> Maybe.map ariaActiveDescendant
        |> Maybe.withDefault noAttribute


viewSuggestion : String -> Suggestion msg -> Html msg
viewSuggestion inputFeltId suggestion =
    li
        [ onClick suggestion.onClick
        , onMouseEnterWithCoords suggestion.onActive
        , role "option"
        , id (suggestionId inputFeltId suggestion)
        , if suggestion.active then
            ariaSelected "true"

          else
            ariaSelected "false"
        ]
        [ span [ classList [ ( "typetext", True ), ( "active", suggestion.active ) ] ]
            [ text suggestion.innhold ]
        ]


onMouseEnterWithCoords : ({ x : Float, y : Float } -> msg) -> Html.Attribute msg
onMouseEnterWithCoords msgConstructor =
    on "mousemove" (mouseMoveDecoder msgConstructor)


mouseMoveDecoder : ({ x : Float, y : Float } -> msg) -> Json.Decode.Decoder msg
mouseMoveDecoder msgConstructor =
    decodeMouseEvent
        |> Json.Decode.map msgConstructor


decodeMouseEvent : Json.Decode.Decoder { x : Float, y : Float }
decodeMouseEvent =
    Json.Decode.succeed MouseCoord
        |> Json.Decode.Pipeline.required "x" Json.Decode.float
        |> Json.Decode.Pipeline.required "y" Json.Decode.float


type alias MouseCoord =
    { x : Float
    , y : Float
    }


suggestionId : String -> Suggestion msg -> String
suggestionId inputId options =
    inputId ++ "-suggestion-" ++ String.replace " " "-" options.innhold


suggestionsId : String -> String
suggestionsId inputId =
    inputId ++ "-suggestions"


ariaOwns : String -> Html.Attribute msg
ariaOwns id =
    Html.Attributes.attribute "aria-owns" id


ariaAutocompleteList : Html.Attribute msg
ariaAutocompleteList =
    Html.Attributes.attribute "aria-autocomplete" "list"


noAttribute : Html.Attribute msg
noAttribute =
    classList []


map : (a -> msg) -> Typeahead a -> Typeahead msg
map msgConstructor (Typeahead options) =
    Typeahead
        { label = options.label
        , innhold = options.innhold
        , inputId = options.inputId
        , feilmelding = options.feilmelding
        , errorMelding = options.errorMelding
        , obligatorisk = options.obligatorisk
        , onInput = options.onInput >> msgConstructor
        , onTypeaheadChange = options.onTypeaheadChange >> msgConstructor
        , prøvIgjenMsg = Maybe.map msgConstructor options.prøvIgjenMsg
        , onFocus = Maybe.map msgConstructor options.onFocus
        , onBlur = Maybe.map msgConstructor options.onBlur
        , suggestions =
            options.suggestions
                |> List.map
                    (\suggestion ->
                        { innhold = suggestion.innhold
                        , onActive = suggestion.onActive >> msgConstructor
                        , onClick = msgConstructor suggestion.onClick
                        , active = suggestion.active
                        }
                    )
        }


innhold : Typeahead msg -> String
innhold (Typeahead options) =
    options.innhold



--- SCROLL TASK ---


scrollActiveSuggestionIntoView : Typeahead msg -> Task Dom.Error ()
scrollActiveSuggestionIntoView (Typeahead options) =
    options
        |> getActiveElement
        |> Maybe.map (getElementsAndScroll options)
        |> Maybe.withDefault (Task.succeed ())


getActiveElement : TypeaheadInfo msg -> Maybe (Task Dom.Error Dom.Element)
getActiveElement options =
    options.suggestions
        |> List.find .active
        |> Maybe.map (suggestionId options.inputId)
        |> Maybe.map Dom.getElement


getElementsAndScroll : TypeaheadInfo msg -> Task Dom.Error Dom.Element -> Task Dom.Error ()
getElementsAndScroll options task =
    task
        |> getSuggestionDomElements options
        |> Task.andThen (scrollIntoView options)


type alias SuggestionDomElements =
    { suggestion : Dom.Element
    , suggestionsViewport : Dom.Viewport
    , suggestionsElement : Dom.Element
    }


getSuggestionDomElements : TypeaheadInfo msg -> Task Dom.Error Dom.Element -> Task Dom.Error SuggestionDomElements
getSuggestionDomElements options task =
    Task.map3
        SuggestionDomElements
        task
        (getViewportOfSuggestions options)
        (getSuggestionsElement options)


getViewportOfSuggestions : TypeaheadInfo msg -> Task Dom.Error Dom.Viewport
getViewportOfSuggestions options =
    options.inputId
        |> suggestionsId
        |> Dom.getViewportOf


getSuggestionsElement : TypeaheadInfo msg -> Task Dom.Error Dom.Element
getSuggestionsElement options =
    options.inputId
        |> suggestionsId
        |> Dom.getElement


scrollIntoView : TypeaheadInfo msg -> SuggestionDomElements -> Task Dom.Error ()
scrollIntoView options domElements =
    if domElements.suggestion.element.y < domElements.suggestionsElement.element.y then
        Dom.setViewportOf (suggestionsId options.inputId) 0 (scrollElementTilØverstY domElements)

    else if domElements.suggestion.element.y + domElements.suggestion.element.height > domElements.suggestionsElement.element.y + domElements.suggestionsElement.element.height then
        Dom.setViewportOf (suggestionsId options.inputId) 0 (scrollElementTilNederstY domElements)

    else
        Task.succeed ()


scrollElementTilNederstY : SuggestionDomElements -> Float
scrollElementTilNederstY domElements =
    let
        yForElementISuggestionsViewport =
            relativYPosisjonForElement domElements
    in
    yForElementISuggestionsViewport + domElements.suggestion.element.height - domElements.suggestionsViewport.viewport.height


scrollElementTilØverstY : SuggestionDomElements -> Float
scrollElementTilØverstY t =
    relativYPosisjonForElement t


relativYPosisjonForElement : SuggestionDomElements -> Float
relativYPosisjonForElement t =
    t.suggestion.element.y - t.suggestionsElement.element.y + t.suggestionsViewport.viewport.y
