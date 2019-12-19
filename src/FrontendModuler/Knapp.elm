module FrontendModuler.Knapp exposing
    ( Enabled(..)
    , Knapp
    , Type(..)
    , innhold
    , knapp
    , msg
    , toHtml
    , withEnabled
    , withMouseDown
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
    , onMouseDown : Maybe msg
    }


type Enabled
    = Enabled
    | Disabled


type Type
    = Normal
    | Hoved
    | Flat


knapp : msg -> String -> Knapp msg
knapp msg_ innhold_ =
    Knapp
        { msg = msg_
        , innhold = innhold_
        , enabled = Enabled
        , knappeType = Normal
        , onMouseDown = Nothing
        }


withEnabled : Enabled -> Knapp msg -> Knapp msg
withEnabled enabled (Knapp options) =
    Knapp { options | enabled = enabled }


withType : Type -> Knapp msg -> Knapp msg
withType knappeType (Knapp options) =
    Knapp { options | knappeType = knappeType }


withMouseDown : msg -> Knapp msg -> Knapp msg
withMouseDown onMouseDown (Knapp options) =
    Knapp { options | onMouseDown = Just onMouseDown }


toHtml : Knapp msg -> Html msg
toHtml (Knapp options) =
    case options.enabled of
        Enabled ->
            button
                [ classList
                    [ ( "Knapp", True )
                    , ( "Knapp--hoved", options.knappeType == Hoved )
                    , ( "Knapp--flat", options.knappeType == Flat )
                    ]
                , onClick options.msg
                , options.onMouseDown
                    |> Maybe.map onMouseDown
                    |> Maybe.withDefault (classList [])
                ]
                [ text options.innhold ]

        Disabled ->
            button
                [ classList [ ( "Knapp", True ), ( "Knapp--disabled", True ), ( "Knapp--hoved", options.knappeType == Hoved ) ]
                , disabled True
                ]
                [ text options.innhold ]


msg : Knapp msg -> msg
msg (Knapp options) =
    options.msg


innhold : Knapp msg -> String
innhold (Knapp options) =
    options.innhold
