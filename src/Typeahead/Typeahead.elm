module Typeahead.Typeahead exposing
    ( GetSuggestionStatus(..)
    , InputStatus(..)
    , Model
    , Msg
    , TypeaheadInitInfo
    , TypeaheadInitWithSelectedInfo
    , getSuggestionsStatus
    , init
    , initWithSelected
    , inputStatus
    , inputValue
    , selected
    , toViewElement
    , update
    , updateSuggestions
    , view
    )

import FrontendModuler.Typeahead as Typeahead exposing (Typeahead)
import Html exposing (..)
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


type GetSuggestionStatus
    = DoNothing
    | GetSuggestionsForInput String


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


update : (a -> String) -> Msg a -> Model a -> ( Model a, Status )
update toString msg (Model model) =
    case msg of
        BrukerOppdatererInput string ->
            ( model.typeaheadState
                |> TypeaheadState.updateValue string
                |> TypeaheadState.showSuggestions
                |> updateTypeaheadState toString model
            , Status
                { getSuggestionStatus = GetSuggestionsForInput string
                , inputStatus = NoChange
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
    ( Model
        { model
            | selected = Just selected_
            , typeaheadState =
                model.typeaheadState
                    |> TypeaheadState.updateValue (toString selected_)
                    |> TypeaheadState.hideSuggestions
        }
    , Status
        { getSuggestionStatus =
            selected_
                |> toString
                |> GetSuggestionsForInput
        , inputStatus = NoChange
        }
    )


updateSuggestions : (a -> String) -> Model a -> List a -> Model a
updateSuggestions toString (Model model) suggestions =
    model.typeaheadState
        |> TypeaheadState.updateSuggestions "" suggestions
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
        |> Typeahead.withOnFocus TypeaheadFikkFokus
        |> Typeahead.withOnBlur TypeaheadMistetFokus
        -- Foreløpig er alle typeaheadfeltene våre obligatoriske, så sender med dette valget uansett
        |> Typeahead.withErObligatorisk


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


init : TypeaheadInitInfo a -> Model a
init input =
    Model
        { selected = Nothing
        , id = input.id
        , label = input.label
        , typeaheadState =
            input.value
                |> TypeaheadState.init
                |> TypeaheadState.hideSuggestions
        }


type alias TypeaheadInitWithSelectedInfo a =
    { selected : a
    , label : String
    , id : String
    , toString : a -> String
    }


initWithSelected : TypeaheadInitWithSelectedInfo a -> Model a
initWithSelected input =
    Model
        { selected = Just input.selected
        , id = input.id
        , label = input.label
        , typeaheadState =
            input.selected
                |> input.toString
                |> TypeaheadState.init
                |> TypeaheadState.hideSuggestions
        }
