module FrontendModuler.DatoInput exposing
    ( DatoInput
    , datoInput
    , toHtml
    , withMaybeFeilmeldingÅr
    , withOnBlurÅr
    )

import Dato exposing (Måned)
import FrontendModuler.Input as Input exposing (Class(..), Input)
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


withMaybeFeilmeldingÅr : Maybe String -> DatoInput msg -> DatoInput msg
withMaybeFeilmeldingÅr feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingÅr = feilmelding }


withOnBlurÅr : msg -> DatoInput msg -> DatoInput msg
withOnBlurÅr onBlur (DatoInput info) =
    DatoInput { info | onBlurÅr = Just onBlur }


toHtml : DatoInput msg -> Html msg
toHtml (DatoInput options) =
    div [ class "DatoInput-wrapper" ]
        [ Select.select
            options.label
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
                |> Input.withClass År
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
