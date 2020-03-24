module FrontendModuler.DatoInput exposing
    ( DatoInput
    , datoInput
    , månedÅrToHtml
    , tilString
    , toHtml
    , withFeilmeldingÅr
    , withFokusId
    , withLabel
    , withOnBlurÅr
    , withWrapperClass
    )

import Dato.Dato as Dato
import Dato.Maned as Måned exposing (Måned)
import FrontendModuler.Input as Input exposing (Input)
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (class)


type DatoInput msg
    = DatoInput (Info msg)


type alias Info msg =
    { label : Maybe String
    , år : String
    , onÅrChange : String -> msg
    , feilmeldingÅr : Maybe String
    , måned : Måned
    , onMånedChange : String -> msg
    , onBlurÅr : Maybe msg
    , wrapperClass : String
    , fokusId : Maybe String
    }


type alias DatoInputInfo msg =
    { år : String
    , onÅrChange : String -> msg
    , måned : Måned
    , onMånedChange : String -> msg
    }


datoInput : DatoInputInfo msg -> DatoInput msg
datoInput { år, onÅrChange, måned, onMånedChange } =
    DatoInput
        { label = Nothing
        , år = år
        , onÅrChange = onÅrChange
        , feilmeldingÅr = Nothing
        , måned = måned
        , onMånedChange = onMånedChange
        , onBlurÅr = Nothing
        , wrapperClass = "DatoInput-skjema-wrapper"
        , fokusId = Nothing
        }


tilString : DatoInput msg -> String
tilString (DatoInput info) =
    case Dato.stringTilÅr info.år of
        Just value ->
            Dato.datoTilString info.måned value

        Nothing ->
            ""


withFeilmeldingÅr : Maybe String -> DatoInput msg -> DatoInput msg
withFeilmeldingÅr feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingÅr = feilmelding }


withOnBlurÅr : msg -> DatoInput msg -> DatoInput msg
withOnBlurÅr onBlur (DatoInput info) =
    DatoInput { info | onBlurÅr = Just onBlur }


withLabel : String -> DatoInput msg -> DatoInput msg
withLabel label_ (DatoInput options) =
    DatoInput { options | label = Just label_ }


withWrapperClass : String -> DatoInput msg -> DatoInput msg
withWrapperClass class (DatoInput info) =
    DatoInput { info | wrapperClass = class }


withFokusId : String -> DatoInput msg -> DatoInput msg
withFokusId id (DatoInput info) =
    DatoInput { info | fokusId = Just id }


withMaybeOnBlur : Maybe msg -> Input msg -> Input msg
withMaybeOnBlur maybeOnBlur input =
    case maybeOnBlur of
        Just onBlur ->
            Input.withOnBlur onBlur input

        Nothing ->
            input


toHtml : DatoInput msg -> Html msg
toHtml (DatoInput options) =
    case options.label of
        Just label ->
            fieldset [ class "DatoInput-fieldset" ]
                [ legend [ class "skjemaelement__label" ]
                    [ text label
                    , span [ class "skjemaelement__måFyllesUt" ] [ text " - må fylles ut" ]
                    ]
                , DatoInput options
                    |> månedÅrToHtml
                ]

        Nothing ->
            DatoInput options
                |> månedÅrToHtml


månedÅrToHtml : DatoInput msg -> Html msg
månedÅrToHtml (DatoInput options) =
    div [ class options.wrapperClass ]
        [ Select.select "Måned"
            options.onMånedChange
            Måned.tilSelectboxOptions
            |> Select.withSelected (Måned.tilString options.måned)
            |> Select.withClass "DatoInput-måned"
            |> Select.withMaybeId options.fokusId
            |> Select.toHtml
        , div [ class "DatoInput-år-wrapper" ]
            [ Input.input { label = "År", msg = options.onÅrChange } options.år
                |> Input.withClass "aar"
                |> Input.withFeilmelding options.feilmeldingÅr
                |> withMaybeOnBlur options.onBlurÅr
                |> Input.toHtml
            ]
        ]
