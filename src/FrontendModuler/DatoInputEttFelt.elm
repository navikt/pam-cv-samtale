module FrontendModuler.DatoInputEttFelt exposing
    ( DatoInputEttFelt
    , datoInputEttFelt
    , toHtml
    , withFeilmelding
    , withId
    , withOnBlur
    , withWrapperClass
    )

import Dato.Dato as Dato
import FrontendModuler.Feilmelding as Feilmelding
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onInput)


type DatoInputEttFelt msg
    = DatoInputEttFelt (Info msg)


type alias Info msg =
    { label : String
    , id : Maybe String
    , dato : String
    , onDatoChange : String -> msg
    , onDatoBlur : Maybe msg
    , feilmelding : Maybe String
    , wrapperClass : String
    }


type alias DatoInputEttFeltInfo msg =
    { label : String
    , dato : String
    , onDatoChange : String -> msg
    }


datoInputEttFelt : DatoInputEttFeltInfo msg -> DatoInputEttFelt msg
datoInputEttFelt { label, dato, onDatoChange } =
    DatoInputEttFelt
        { label = label
        , dato = dato
        , onDatoChange = onDatoChange
        , onDatoBlur = Nothing
        , feilmelding = Nothing
        , id = Nothing
        , wrapperClass = ""
        }


withFeilmelding : Maybe String -> DatoInputEttFelt msg -> DatoInputEttFelt msg
withFeilmelding feilmelding (DatoInputEttFelt info) =
    DatoInputEttFelt { info | feilmelding = feilmelding }


withId : String -> DatoInputEttFelt msg -> DatoInputEttFelt msg
withId id (DatoInputEttFelt info) =
    DatoInputEttFelt { info | id = Just id }


withWrapperClass : String -> DatoInputEttFelt msg -> DatoInputEttFelt msg
withWrapperClass class (DatoInputEttFelt info) =
    DatoInputEttFelt { info | wrapperClass = class }


toHtml : DatoInputEttFelt msg -> Html msg
toHtml (DatoInputEttFelt options) =
    div [ class ("skjemaelement " ++ options.wrapperClass) ]
        [ label []
            [ span [ class "skjemaelement__label " ]
                [ text options.label ]
            , Html.input
                [ type_ "text"
                , value options.dato
                , classList
                    [ ( "skjemaelement__input", True )
                    , ( "input--m", True )
                    , ( "skjemaelement__input--harFeil", options.feilmelding /= Nothing )
                    ]
                , placeholder "dd.mm.åååå"
                , onInput options.onDatoChange
                , options.onDatoBlur
                    |> Maybe.map onBlur
                    |> Maybe.withDefault noAttribute
                , options.id
                    |> Maybe.map id
                    |> Maybe.withDefault noAttribute
                ]
                []
            ]
        , Feilmelding.htmlFeilmelding options.feilmelding
        ]


withOnBlur : msg -> DatoInputEttFelt msg -> DatoInputEttFelt msg
withOnBlur onBlur (DatoInputEttFelt info) =
    DatoInputEttFelt { info | onDatoBlur = Just onBlur }


noAttribute : Html.Attribute msg
noAttribute =
    classList []
