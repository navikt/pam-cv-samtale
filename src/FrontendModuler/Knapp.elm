module FrontendModuler.Knapp exposing
    ( Class(..)
    , Enabled(..)
    , Knapp
    , Type(..)
    , knapp
    , toHtml
    , withClass
    , withEnabled
    , withType
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Knapp msg
    = Knapp (Options msg)


type alias Options msg =
    { msg : msg
    , innhold : String
    , enabled : Enabled
    , knappeType : Type
    , class : Maybe Class
    }


type Enabled
    = Enabled
    | Disabled


type Type
    = Normal
    | Hoved


type Class
    = MånedKnapp


knapp : msg -> String -> Knapp msg
knapp msg innhold =
    Knapp
        { msg = msg
        , innhold = innhold
        , enabled = Enabled
        , knappeType = Normal
        , class = Nothing
        }


withClass : Class -> Knapp msg -> Knapp msg
withClass class (Knapp options) =
    Knapp { options | class = Just class }


withEnabled : Enabled -> Knapp msg -> Knapp msg
withEnabled enabled (Knapp options) =
    Knapp { options | enabled = enabled }


withType : Type -> Knapp msg -> Knapp msg
withType knappeType (Knapp options) =
    Knapp { options | knappeType = knappeType }


toHtml : Knapp msg -> Html msg
toHtml (Knapp options) =
    case options.class of
        Just månedknapp ->
            case options.enabled of
                Enabled ->
                    button
                        [ classList [ ( "Knapp", True ), ( "Knapp--hoved", options.knappeType == Hoved ), ( "månedknapp", True ) ]
                        , onClick options.msg
                        ]
                        [ text options.innhold ]

                Disabled ->
                    button
                        [ classList [ ( "Knapp", True ), ( "Knapp--disabled", True ), ( "Knapp--hoved", options.knappeType == Hoved ) ]
                        , disabled True
                        ]
                        [ text options.innhold ]

        Nothing ->
            case options.enabled of
                Enabled ->
                    button
                        [ classList [ ( "Knapp", True ), ( "Knapp--hoved", options.knappeType == Hoved ) ]
                        , onClick options.msg
                        ]
                        [ text options.innhold ]

                Disabled ->
                    button
                        [ classList [ ( "Knapp", True ), ( "Knapp--disabled", True ), ( "Knapp--hoved", options.knappeType == Hoved ) ]
                        , disabled True
                        ]
                        [ text options.innhold ]
