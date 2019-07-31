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
    , class : Maybe ClassInfo
    }


type alias ClassInfo =
    { classType : Class
    , className : String
    }


type Enabled
    = Enabled
    | Disabled


type Type
    = Normal
    | Hoved


type Class
    = MånedKnapp
    | UtdanningsNivåKnapp
    | SpråknivåKnapp


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
    case class of
        MånedKnapp ->
            Knapp { options | class = Just { classType = MånedKnapp, className = "månedknapp" } }

        UtdanningsNivåKnapp ->
            Knapp { options | class = Just { classType = UtdanningsNivåKnapp, className = "utdanningsnivåknapp" } }

        SpråknivåKnapp ->
            Knapp { options | class = Just { classType = SpråknivåKnapp, className = "språknivåknapp" } }


withEnabled : Enabled -> Knapp msg -> Knapp msg
withEnabled enabled (Knapp options) =
    Knapp { options | enabled = enabled }


withType : Type -> Knapp msg -> Knapp msg
withType knappeType (Knapp options) =
    Knapp { options | knappeType = knappeType }


toHtml : Knapp msg -> Html msg
toHtml (Knapp options) =
    case options.class of
        Just classInfo ->
            case options.enabled of
                Enabled ->
                    button
                        [ classList [ ( "Knapp", True ), ( "Knapp--hoved", options.knappeType == Hoved ), ( classInfo.className, True ) ]
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
