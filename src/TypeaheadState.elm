module TypeaheadState exposing
    ( ActiveState(..)
    , TypeaheadState
    , arrowDown
    , arrowUp
    , findSuggestionMatchingInputValue
    , getActive
    , hideSuggestions
    , init
    , mapSuggestions
    , removeActive
    , showSuggestions
    , updateActive
    , updateSuggestions
    , updateValue
    , value
    )

import List.Extra as List


type TypeaheadState a
    = TypeaheadState
        { value : String
        , suggestions : SuggestionList a
        , showSuggestions : Bool
        }


init : String -> TypeaheadState a
init value_ =
    TypeaheadState
        { value = value_
        , suggestions = TomListe
        , showSuggestions = True
        }


value : TypeaheadState a -> String
value (TypeaheadState info) =
    info.value


getActive : TypeaheadState a -> Maybe a
getActive (TypeaheadState info) =
    case info.suggestions of
        TomListe ->
            Nothing

        IngenMarkert _ ->
            Nothing

        SuggestionMarkert { active } ->
            Just active


updateValue : String -> TypeaheadState a -> TypeaheadState a
updateValue value_ (TypeaheadState info) =
    TypeaheadState { info | value = value_ }


updateSuggestions : String -> List a -> TypeaheadState a -> TypeaheadState a
updateSuggestions value_ suggestions (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions = IngenMarkert suggestions
        }


updateActive : a -> TypeaheadState a -> TypeaheadState a
updateActive newActive (TypeaheadState info) =
    TypeaheadState { info | suggestions = updateActiveSuggestion newActive info.suggestions }


updateActiveSuggestion : a -> SuggestionList a -> SuggestionList a
updateActiveSuggestion newActive suggestionList =
    case suggestionList of
        TomListe ->
            TomListe

        IngenMarkert list ->
            makeActive newActive list

        SuggestionMarkert { before, active, after } ->
            [ before, [ active ], after ]
                |> List.concat
                |> makeActive newActive


makeActive : a -> List a -> SuggestionList a
makeActive newActive suggestions =
    let
        findActive : a -> { before : List a, maybeActive : Maybe a, after : List a } -> { before : List a, maybeActive : Maybe a, after : List a }
        findActive elem { before, maybeActive, after } =
            case maybeActive of
                Just a ->
                    { before = before
                    , maybeActive = maybeActive
                    , after = after ++ [ elem ]
                    }

                Nothing ->
                    if elem == newActive then
                        { before = before
                        , maybeActive = Just elem
                        , after = after
                        }

                    else
                        { before = before ++ [ elem ]
                        , maybeActive = Nothing
                        , after = after
                        }

        maybeMarkertState =
            List.foldl findActive { before = [], maybeActive = Nothing, after = [] } suggestions
    in
    case maybeMarkertState.maybeActive of
        Just elem ->
            SuggestionMarkert
                { before = maybeMarkertState.before
                , active = elem
                , after = maybeMarkertState.after
                }

        Nothing ->
            if List.isEmpty maybeMarkertState.before then
                TomListe

            else
                IngenMarkert maybeMarkertState.before


arrowDown : TypeaheadState a -> TypeaheadState a
arrowDown (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions =
                case info.suggestions of
                    TomListe ->
                        TomListe

                    IngenMarkert [] ->
                        TomListe

                    IngenMarkert (first :: rest) ->
                        SuggestionMarkert
                            { before = []
                            , active = first
                            , after = rest
                            }

                    SuggestionMarkert { before, active, after } ->
                        case after of
                            first :: rest ->
                                SuggestionMarkert
                                    { before = before ++ [ active ]
                                    , active = first
                                    , after = rest
                                    }

                            [] ->
                                [ before, [ active ], after ]
                                    |> List.concat
                                    |> IngenMarkert
        }


arrowUp : TypeaheadState a -> TypeaheadState a
arrowUp (TypeaheadState info) =
    TypeaheadState
        { info
            | suggestions =
                case info.suggestions of
                    TomListe ->
                        TomListe

                    IngenMarkert list ->
                        case List.reverse list of
                            last :: rest ->
                                SuggestionMarkert
                                    { before = List.reverse rest
                                    , active = last
                                    , after = []
                                    }

                            [] ->
                                TomListe

                    SuggestionMarkert { before, active, after } ->
                        case List.reverse before of
                            last :: rest ->
                                SuggestionMarkert
                                    { before = List.reverse rest
                                    , active = last
                                    , after = active :: after
                                    }

                            [] ->
                                [ before, [ active ], after ]
                                    |> List.concat
                                    |> IngenMarkert
        }


removeActive : TypeaheadState a -> TypeaheadState a
removeActive (TypeaheadState info) =
    TypeaheadState { info | suggestions = removeActiveFromSuggestions info.suggestions }


removeActiveFromSuggestions : SuggestionList a -> SuggestionList a
removeActiveFromSuggestions suggestionList =
    case suggestionList of
        TomListe ->
            TomListe

        IngenMarkert suggestions ->
            IngenMarkert suggestions

        SuggestionMarkert { before, active, after } ->
            [ before, [ active ], after ]
                |> List.concat
                |> IngenMarkert


showSuggestions : TypeaheadState a -> TypeaheadState a
showSuggestions (TypeaheadState info) =
    TypeaheadState { info | showSuggestions = True }


hideSuggestions : TypeaheadState a -> TypeaheadState a
hideSuggestions (TypeaheadState info) =
    TypeaheadState { info | showSuggestions = False }


findSuggestionMatchingInputValue : (a -> String) -> TypeaheadState a -> Maybe a
findSuggestionMatchingInputValue toString (TypeaheadState info) =
    let
        predicate suggestion =
            (toString >> String.trim >> String.toLower) suggestion == (String.trim >> String.toLower) info.value
    in
    case info.suggestions of
        TomListe ->
            Nothing

        IngenMarkert suggestions ->
            List.find predicate suggestions

        SuggestionMarkert { before, active, after } ->
            [ before
            , [ active ]
            , after
            ]
                |> List.concat
                |> List.find predicate


type ActiveState
    = Active
    | NotActive


mapSuggestions : (ActiveState -> a -> b) -> TypeaheadState a -> List b
mapSuggestions function (TypeaheadState info) =
    if info.showSuggestions then
        case info.suggestions of
            TomListe ->
                []

            IngenMarkert suggestions ->
                List.map (function NotActive) suggestions

            SuggestionMarkert { before, active, after } ->
                List.concat
                    [ List.map (function NotActive) before
                    , [ function Active active ]
                    , List.map (function NotActive) after
                    ]

    else
        []


type SuggestionList a
    = TomListe
    | IngenMarkert (List a)
    | SuggestionMarkert
        { before : List a
        , active : a
        , after : List a
        }
