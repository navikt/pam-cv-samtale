module FrontendModuler.DatoInput exposing
    ( DatoInput
    , datoInput
    , toHtml
    , withFeilmeldingÅr
    , withOnBlurÅr
    )

import Dato exposing (Måned)
import FrontendModuler.Input as Input exposing (Input)
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (class)


type DatoInput msg
    = DatoInput (Info msg)


type alias Info msg =
    { label : String
    , år : String
    , onÅrChange : String -> msg
    , feilmeldingÅr : Maybe String
    , måned : Måned
    , onMånedChange : String -> msg
    , onBlurÅr : Maybe msg
    }


type alias DatoInputInfo msg =
    { label : String
    , år : String
    , onÅrChange : String -> msg
    , måned : Måned
    , onMånedChange : String -> msg
    }


datoInput : DatoInputInfo msg -> DatoInput msg
datoInput { label, år, onÅrChange, måned, onMånedChange } =
    DatoInput
        { label = label
        , år = år
        , onÅrChange = onÅrChange
        , feilmeldingÅr = Nothing
        , måned = måned
        , onMånedChange = onMånedChange
        , onBlurÅr = Nothing
        }


withFeilmeldingÅr : Maybe String -> DatoInput msg -> DatoInput msg
withFeilmeldingÅr feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingÅr = feilmelding }


withOnBlurÅr : msg -> DatoInput msg -> DatoInput msg
withOnBlurÅr onBlur (DatoInput info) =
    DatoInput { info | onBlurÅr = Just onBlur }


toHtml : DatoInput msg -> Html msg
toHtml (DatoInput options) =
    fieldset [ class "DatoInput-fieldset" ]
        [ legend [ class "skjemaelement__label" ]
            [ text options.label
            , span [ class "skjemaelement__måFyllesUt" ] [ text " - må fylles ut" ]
            ]
        , div [ class "DatoInput-wrapper" ]
            [ Select.select
                "Måned"
                options.onMånedChange
                [ ( "Januar", "Januar" )
                , ( "Februar", "Februar" )
                , ( "Mars", "Mars" )
                , ( "April", "April" )
                , ( "Mai", "Mai" )
                , ( "Juni", "Juni" )
                , ( "Juli", "Juli" )
                , ( "August", "August" )
                , ( "September", "September" )
                , ( "Oktober", "Oktober" )
                , ( "November", "November" )
                , ( "Desember", "Desember" )
                ]
                |> Select.withSelected (Dato.månedTilString options.måned)
                |> Select.withClass "DatoInput-måned"
                |> Select.toHtml
            , div [ class "DatoInput-år-wrapper" ]
                [ Input.input { label = "År", msg = options.onÅrChange } options.år
                    |> Input.withClass "aar"
                    |> Input.withFeilmelding options.feilmeldingÅr
                    |> withMaybeOnBlur options.onBlurÅr
                    |> Input.toHtml
                ]
            ]
        ]


withMaybeOnBlur : Maybe msg -> Input msg -> Input msg
withMaybeOnBlur maybeOnBlur input =
    case maybeOnBlur of
        Just onBlur ->
            Input.withOnBlur onBlur input

        Nothing ->
            input
