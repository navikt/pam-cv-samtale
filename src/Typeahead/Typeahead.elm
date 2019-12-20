module Typeahead.Typeahead exposing
    ( GetSuggestionStatus(..)
    , InputStatus(..)
    , Model
    , Msg
    , Query
    , TypeaheadInitInfo
    , TypeaheadInitWithSelectedInfo
    , getSuggestionsStatus
    , init
    , initWithSelected
    , inputStatus
    , inputValue
    , queryToString
    , selected
    , toViewElement
    , update
    , updateSuggestions
    , view
    )

import ErrorHandtering as ErrorHåndtering
import FrontendModuler.Typeahead as Typeahead exposing (Typeahead)
import Html exposing (..)
import Http
import Typeahead.TypeaheadState as TypeaheadState exposing (TypeaheadState)



--- MODEL ---


type Model a
    = Model (ModelInfo a)


type alias ModelInfo a =
    { selected : Maybe a
    , typeaheadState : TypeaheadState a
    , id : String
    , label : String
    }


inputValue : Model a -> String
inputValue (Model model) =
    TypeaheadState.value model.typeaheadState


selected : Model a -> Maybe a
selected (Model model) =
    model.selected


type Query
    = Query TypeaheadState.Query


queryToString : Query -> String
queryToString (Query typeaheadStateQuery) =
    TypeaheadState.queryToString typeaheadStateQuery


type GetSuggestionStatus
    = DoNothing
    | GetSuggestionsForInput Query


type InputStatus
    = Submit
    | InputBlurred
    | NoChange


type Status
    = Status
        { getSuggestionStatus : GetSuggestionStatus
        , inputStatus : InputStatus
        }


getSuggestionsStatus : Status -> GetSuggestionStatus
getSuggestionsStatus (Status info) =
    info.getSuggestionStatus


inputStatus : Status -> InputStatus
inputStatus (Status info) =
    info.inputStatus



--- UPDATE ---


type Msg a
    = BrukerOppdatererInput String
    | BrukerTrykkerTypeaheadTast Typeahead.Operation
    | BrukerHovrerOverTypeaheadSuggestion a
    | BrukerVelgerElement a
    | TypeaheadFikkFokus
    | TypeaheadMistetFokus
    | BrukerTrykketPåPrøvIgjen


update : (a -> String) -> Msg a -> Model a -> ( Model a, Status )
update toString msg (Model model) =
    case msg of
        BrukerOppdatererInput string ->
            let
                newTypeaheadState =
                    model.typeaheadState
                        |> TypeaheadState.updateValue string
                        |> TypeaheadState.showSuggestions
            in
            ( newTypeaheadState
                |> updateTypeaheadState toString model
            , Status
                { inputStatus = NoChange
                , getSuggestionStatus =
                    newTypeaheadState
                        |> TypeaheadState.query
                        |> Query
                        |> GetSuggestionsForInput
                }
            )

        BrukerTrykkerTypeaheadTast operation ->
            case operation of
                Typeahead.ArrowUp ->
                    ( model.typeaheadState
                        |> TypeaheadState.arrowUp
                        |> updateTypeaheadState toString model
                    , Status
                        { getSuggestionStatus = DoNothing
                        , inputStatus = NoChange
                        }
                    )

                Typeahead.ArrowDown ->
                    ( model.typeaheadState
                        |> TypeaheadState.arrowDown
                        |> updateTypeaheadState toString model
                    , Status
                        { getSuggestionStatus = DoNothing
                        , inputStatus = NoChange
                        }
                    )

                Typeahead.Enter ->
                    case TypeaheadState.active model.typeaheadState of
                        Just active ->
                            updateAfterSelect toString active model

                        Nothing ->
                            ( model.typeaheadState
                                |> TypeaheadState.hideSuggestions
                                |> updateTypeaheadState toString model
                            , Status
                                { getSuggestionStatus = DoNothing
                                , inputStatus = Submit
                                }
                            )

                Typeahead.MouseLeaveSuggestions ->
                    ( model.typeaheadState
                        |> TypeaheadState.removeActive
                        |> updateTypeaheadState toString model
                    , Status
                        { getSuggestionStatus = DoNothing
                        , inputStatus = NoChange
                        }
                    )

        BrukerHovrerOverTypeaheadSuggestion active ->
            ( model.typeaheadState
                |> TypeaheadState.updateActive active
                |> updateTypeaheadState toString model
            , Status
                { getSuggestionStatus = DoNothing
                , inputStatus = NoChange
                }
            )

        BrukerVelgerElement selected_ ->
            updateAfterSelect toString selected_ model

        TypeaheadFikkFokus ->
            ( model.typeaheadState
                |> TypeaheadState.showSuggestions
                |> updateTypeaheadState toString model
            , Status
                { getSuggestionStatus = DoNothing
                , inputStatus = NoChange
                }
            )

        TypeaheadMistetFokus ->
            ( model.typeaheadState
                |> TypeaheadState.hideSuggestions
                |> updateTypeaheadState toString model
            , Status
                { getSuggestionStatus = DoNothing
                , inputStatus = InputBlurred
                }
            )

        BrukerTrykketPåPrøvIgjen ->
            ( Model model
            , Status
                { inputStatus = NoChange
                , getSuggestionStatus =
                    model.typeaheadState
                        |> TypeaheadState.query
                        |> Query
                        |> GetSuggestionsForInput
                }
            )


updateTypeaheadState : (a -> String) -> ModelInfo a -> TypeaheadState a -> Model a
updateTypeaheadState toString model typeaheadState =
    Model
        { model
            | typeaheadState = typeaheadState
            , selected =
                case model.selected of
                    Just selected_ ->
                        if toString selected_ == TypeaheadState.value typeaheadState then
                            Just selected_

                        else
                            TypeaheadState.findSuggestionMatchingInputValue toString typeaheadState

                    Nothing ->
                        TypeaheadState.findSuggestionMatchingInputValue toString typeaheadState
        }


updateAfterSelect : (a -> String) -> a -> ModelInfo a -> ( Model a, Status )
updateAfterSelect toString selected_ model =
    let
        newTypeaheadState =
            model.typeaheadState
                |> TypeaheadState.updateValue (toString selected_)
                |> TypeaheadState.hideSuggestions
    in
    ( Model
        { model
            | selected = Just selected_
            , typeaheadState = newTypeaheadState
        }
    , Status
        { getSuggestionStatus =
            newTypeaheadState
                |> TypeaheadState.query
                |> Query
                |> GetSuggestionsForInput
        , inputStatus = NoChange
        }
    )


updateSuggestions : (a -> String) -> Model a -> Query -> Result Http.Error (List a) -> Model a
updateSuggestions toString (Model model) (Query query) suggestions =
    model.typeaheadState
        |> TypeaheadState.updateSuggestions query suggestions
        |> updateTypeaheadState toString model



--- VIEW ---


view : (a -> String) -> Model a -> Maybe String -> Html (Msg a)
view toString model feilmelding =
    toViewElement toString model feilmelding
        |> Typeahead.toHtml


toViewElement : (a -> String) -> Model a -> Maybe String -> Typeahead (Msg a)
toViewElement toString (Model model) feilmelding =
    model.typeaheadState
        |> TypeaheadState.value
        |> Typeahead.typeahead { label = model.label, onInput = BrukerOppdatererInput, onTypeaheadChange = BrukerTrykkerTypeaheadTast, inputId = model.id }
        |> Typeahead.withSuggestions (viewSuggestion toString model.typeaheadState)
        |> Typeahead.withFeilmelding feilmelding
        |> Typeahead.withErrorMelding (errorMelding model.typeaheadState)
        |> Typeahead.withPrøvIgjenKnapp (prøvPåNyttMsg model.typeaheadState)
        |> Typeahead.withOnFocus TypeaheadFikkFokus
        |> Typeahead.withOnBlur TypeaheadMistetFokus
        -- Foreløpig er alle typeaheadfeltene våre obligatoriske, så sender med dette valget uansett
        |> Typeahead.withErObligatorisk


errorMelding : TypeaheadState a -> Maybe String
errorMelding typeaheadState =
    typeaheadState
        |> TypeaheadState.error
        |> Maybe.map ErrorHåndtering.feilmeldingTypeahead


prøvPåNyttMsg : TypeaheadState a -> Maybe (Msg a)
prøvPåNyttMsg typeaheadState =
    typeaheadState
        |> TypeaheadState.error
        |> Maybe.map ErrorHåndtering.prøvPåNyttEtterTypeaheadError
        |> Maybe.andThen
            (\prøvPåNytt ->
                if prøvPåNytt then
                    Just BrukerTrykketPåPrøvIgjen

                else
                    Nothing
            )


viewSuggestion : (a -> String) -> TypeaheadState a -> List (Typeahead.Suggestion (Msg a))
viewSuggestion toString typeaheadState =
    typeaheadState
        |> TypeaheadState.mapSuggestions
            (\activeState suggestion ->
                { innhold = toString suggestion
                , onClick = BrukerVelgerElement suggestion
                , onActive = BrukerHovrerOverTypeaheadSuggestion suggestion
                , active =
                    case activeState of
                        TypeaheadState.Active ->
                            True

                        TypeaheadState.NotActive ->
                            False
                }
            )



--- INIT ---


type alias TypeaheadInitInfo a =
    { value : String
    , label : String
    , id : String
    , toString : a -> String
    }


init : TypeaheadInitInfo a -> ( Model a, Query )
init input =
    let
        typeaheadState =
            input.value
                |> TypeaheadState.init
                |> TypeaheadState.hideSuggestions
    in
    ( Model
        { selected = Nothing
        , id = input.id
        , label = input.label
        , typeaheadState = typeaheadState
        }
    , typeaheadState
        |> TypeaheadState.query
        |> Query
    )


type alias TypeaheadInitWithSelectedInfo a =
    { selected : a
    , label : String
    , id : String
    , toString : a -> String
    }


initWithSelected : TypeaheadInitWithSelectedInfo a -> ( Model a, Query )
initWithSelected input =
    let
        typeaheadState =
            input.selected
                |> input.toString
                |> TypeaheadState.init
                |> TypeaheadState.hideSuggestions
    in
    ( Model
        { selected = Just input.selected
        , id = input.id
        , label = input.label
        , typeaheadState =
            input.selected
                |> input.toString
                |> TypeaheadState.init
                |> TypeaheadState.hideSuggestions
        }
    , typeaheadState
        |> TypeaheadState.query
        |> Query
    )
