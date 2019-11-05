module FrontendModuler.ValgfriDatoInput exposing
    ( DatoInput
    , datoInput
    , toHtml
    , withMaybeFeilmeldingMåned
    , withMaybeFeilmeldingPeriode
    , withMaybeFeilmeldingÅr
    , withOnBlurÅr
    )

import Dato exposing (Måned)
import FrontendModuler.Feilmelding exposing (htmlFeilmelding)
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
    , feilmeldingPeriode : Maybe String
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
        , feilmeldingPeriode = Nothing
        , onBlurÅr = Nothing
        }


feilmeldingPeriode : DatoInput msg -> Maybe String
feilmeldingPeriode (DatoInput info) =
    if info.feilmeldingMåned == Nothing && info.feilmeldingÅr == Nothing then
        info.feilmeldingPeriode

    else
        Nothing


feilÅrIPeriode : DatoInput msg -> Bool
feilÅrIPeriode (DatoInput info) =
    case info.feilmeldingPeriode of
        Just _ ->
            if info.måned /= Nothing then
                -- Hvis måned er fylt ut, marker år som rødt
                True

            else
                False

        _ ->
            False


feilMånedIPeriode : DatoInput msg -> Bool
feilMånedIPeriode (DatoInput info) =
    case feilmeldingPeriode (DatoInput info) of
        Just _ ->
            if info.måned == Nothing then
                -- Hvis måned ikke er fylt ut, marker den som rødt
                True

            else
                False

        _ ->
            False


withMaybeFeilmeldingÅr : Maybe String -> DatoInput msg -> DatoInput msg
withMaybeFeilmeldingÅr feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingÅr = feilmelding }


withMaybeFeilmeldingMåned : Maybe String -> DatoInput msg -> DatoInput msg
withMaybeFeilmeldingMåned feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingMåned = feilmelding }


withMaybeFeilmeldingPeriode : Maybe String -> DatoInput msg -> DatoInput msg
withMaybeFeilmeldingPeriode feilmelding (DatoInput info) =
    DatoInput { info | feilmeldingPeriode = feilmelding }


withOnBlurÅr : msg -> DatoInput msg -> DatoInput msg
withOnBlurÅr onBlur (DatoInput info) =
    DatoInput { info | onBlurÅr = Just onBlur }


toHtml : DatoInput msg -> Html msg
toHtml (DatoInput options) =
    div [ class "ValgfriDatoInput-kolonne" ]
        [ div [ class "Dato-wrapper" ]
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
                |> Select.withClass
                    ("DatoInput-måned"
                        ++ (if feilMånedIPeriode (DatoInput options) then
                                " skjemaelement__input--harFeil"

                            else
                                ""
                           )
                    )
                |> Select.withMaybeFeilmelding options.feilmeldingMåned
                |> Select.toHtml
            , div [ class "DatoInput-år-wrapper" ]
                [ Input.input { label = "År", msg = options.onÅrChange } options.år
                    |> Input.withClass
                        ("aar"
                            ++ (if feilÅrIPeriode (DatoInput options) then
                                    " skjemaelement__input--harFeil"

                                else
                                    ""
                               )
                        )
                    |> Input.withMaybeFeilmelding options.feilmeldingÅr
                    |> withMaybeOnBlur options.onBlurÅr
                    |> Input.toHtml
                ]
            ]
        , htmlFeilmelding (feilmeldingPeriode (DatoInput options))
        ]


withMaybeOnBlur : Maybe msg -> Input msg -> Input msg
withMaybeOnBlur maybeOnBlur input =
    case maybeOnBlur of
        Just onBlur ->
            Input.withOnBlur onBlur input

        Nothing ->
            input
