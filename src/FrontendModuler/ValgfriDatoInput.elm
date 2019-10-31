module FrontendModuler.ValgfriDatoInput exposing
    ( DatoInput
    , datoInput
    , toHtml
    , withMaybeFeilmeldingMåned
    , withMaybeFeilmeldingÅr
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
    , måned : Maybe Måned
    , år : String
    , onMånedChange : String -> msg
    , onÅrChange : String -> msg
    , feilmeldingMåned : Maybe String
    , feilmeldingÅr : Maybe String
    , onBlurÅr : Maybe msg
    }


type alias DatoInputInfo msg =
    { label : String
    , år : String
    , onÅrChange : String -> msg
    , måned : Maybe Måned
    , onMånedChange : String -> msg
    }


datoInput : DatoInputInfo msg -> DatoInput msg
datoInput { label, år, onÅrChange, måned, onMånedChange } =
    DatoInput
        { label = label
        , måned = måned
        , år = år
        , onMånedChange = onMånedChange
        , onÅrChange = onÅrChange
        , feilmeldingMåned = Nothing
        , feilmeldingÅr = Nothing
        , onBlurÅr = Nothing
        }


withMaybeFeilmeldingÅr : Maybe String -> DatoInput msg -> DatoInput msg
withMaybeFeilmeldingÅr feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingÅr = feilmelding }


withMaybeFeilmeldingMåned : Maybe String -> DatoInput msg -> DatoInput msg
withMaybeFeilmeldingMåned feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingMåned = feilmelding }


withOnBlurÅr : msg -> DatoInput msg -> DatoInput msg
withOnBlurÅr onBlur (DatoInput info) =
    DatoInput { info | onBlurÅr = Just onBlur }


toHtml : DatoInput msg -> Html msg
toHtml (DatoInput options) =
    div [ class "DatoInput-wrapper" ]
        [ Select.select
            options.label
            options.onMånedChange
            [ ( "Måned", "Måned" )
            , ( "Januar", "Januar" )
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
            |> Select.withSelected
                (case options.måned of
                    Nothing ->
                        "Måned"

                    Just måned_ ->
                        Dato.månedTilString måned_
                )
            |> Select.withClass "DatoInput-måned"
            |> Select.withMaybeFeilmelding options.feilmeldingMåned
            |> Select.toHtml
        , div [ class "DatoInput-år-wrapper" ]
            [ Input.input { label = "År", msg = options.onÅrChange } options.år
                |> Input.withClass "aar"
                |> Input.withMaybeFeilmelding options.feilmeldingÅr
                |> withMaybeOnBlur options.onBlurÅr
                |> Input.toHtml
            ]
        ]


withMaybeOnBlur : Maybe msg -> Input msg -> Input msg
withMaybeOnBlur maybeOnBlur input =
    case maybeOnBlur of
        Just onBlur ->
            Input.withOnBlur onBlur input

        Nothing ->
            input
