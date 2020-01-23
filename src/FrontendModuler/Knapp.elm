module FrontendModuler.Knapp exposing
    ( Enabled(..)
    , Knapp
    , Type(..)
    , innhold
    , knapp
    , msg
    , toHtml
    , withAttribute
    , withClass
    , withEnabled
    , withId
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
    , classes : List String
    , knappeType : Type
    , onMouseDown : Maybe msg
    , extraAttributes : List (Html.Attribute msg)
    , id : Maybe String
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
        , classes = []
        , knappeType = Normal
        , onMouseDown = Nothing
        , extraAttributes = []
        , id = Nothing
        }


withId : String -> Knapp msg -> Knapp msg
withId id (Knapp options) =
    Knapp { options | id = Just id }


withClass : String -> Knapp msg -> Knapp msg
withClass class (Knapp options) =
    Knapp { options | classes = class :: options.classes }


withEnabled : Enabled -> Knapp msg -> Knapp msg
withEnabled enabled (Knapp options) =
    Knapp { options | enabled = enabled }


withType : Type -> Knapp msg -> Knapp msg
withType knappeType (Knapp options) =
    Knapp { options | knappeType = knappeType }


withMouseDown : msg -> Knapp msg -> Knapp msg
withMouseDown onMouseDown (Knapp options) =
    Knapp { options | onMouseDown = Just onMouseDown }


withAttribute : Html.Attribute msg -> Knapp msg -> Knapp msg
withAttribute attribute (Knapp options) =
    Knapp { options | extraAttributes = attribute :: options.extraAttributes }


toHtml : Knapp msg -> Html msg
toHtml (Knapp options) =
    case options.enabled of
        Enabled ->
            button
                (List.concat
                    [ options.extraAttributes
                    , [ classList
                            [ ( "Knapp", True )
                            , ( "Knapp--hoved", options.knappeType == Hoved )
                            , ( "Knapp--flat", options.knappeType == Flat )
                            ]
                      , classList (List.map (\class -> ( class, True )) options.classes)
                      , onClick options.msg
                      , options.onMouseDown
                            |> Maybe.map onMouseDown
                            |> Maybe.withDefault noAttribute
                      , options.id
                            |> Maybe.map id
                            |> Maybe.withDefault noAttribute
                      ]
                    ]
                )
                [ text options.innhold ]

        Disabled ->
            button
                (List.concat
                    [ options.extraAttributes
                    , [ classList
                            [ ( "Knapp", True )
                            , ( "Knapp--disabled", True )
                            , ( "Knapp--hoved", options.knappeType == Hoved )
                            ]
                      , disabled True
                      ]
                    ]
                )
                [ text options.innhold ]


msg : Knapp msg -> msg
msg (Knapp options) =
    options.msg


innhold : Knapp msg -> String
innhold (Knapp options) =
    options.innhold


noAttribute : Html.Attribute msg
noAttribute =
    classList []
