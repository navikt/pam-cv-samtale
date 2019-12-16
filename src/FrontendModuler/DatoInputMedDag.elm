module FrontendModuler.DatoInputMedDag exposing
    ( DatoInputMedDag
    , datoInputMedDag
    , toHtml
    , withFeilmelding
    , withId
    )

import Dato exposing (DatoFeilmelding)
import Dato.Maned as Måned exposing (Måned)
import FrontendModuler.Input as Input exposing (Input)
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes.Aria exposing (ariaLive, role)
import Html.Events exposing (onBlur, onInput)


type DatoInputMedDag msg
    = DatoInputMedDag (Info msg)


type alias Info msg =
    { label : String
    , år : String
    , onÅrChange : String -> msg
    , måned : Maybe Måned
    , onMånedChange : String -> msg
    , id : Maybe String
    , dag : String
    , onDagChange : String -> msg
    , feilmelding : Maybe DatoFeilmelding
    }


type alias DatoInputMedDagInfo msg =
    { label : String
    , år : String
    , onÅrChange : String -> msg
    , måned : Maybe Måned
    , onMånedChange : String -> msg
    , dag : String
    , onDagChange : String -> msg
    }


datoInputMedDag : DatoInputMedDagInfo msg -> DatoInputMedDag msg
datoInputMedDag { label, år, onÅrChange, måned, onMånedChange, dag, onDagChange } =
    DatoInputMedDag
        { label = label
        , år = år
        , onÅrChange = onÅrChange
        , måned = måned
        , onMånedChange = onMånedChange
        , dag = dag
        , onDagChange = onDagChange
        , feilmelding = Nothing
        , id = Nothing
        }


withFeilmelding : Maybe DatoFeilmelding -> DatoInputMedDag msg -> DatoInputMedDag msg
withFeilmelding feilmelding (DatoInputMedDag info) =
    DatoInputMedDag { info | feilmelding = feilmelding }


withId : String -> DatoInputMedDag msg -> DatoInputMedDag msg
withId id (DatoInputMedDag info) =
    DatoInputMedDag { info | id = Just id }


toHtml : DatoInputMedDag msg -> Html msg
toHtml (DatoInputMedDag options) =
    div []
        [ label [] [ text options.label ]
        , div []
            [ div [ class "ForerkortSeksjon-datolinje" ]
                [ div [ class "DatoInputMedDag-dag-wrapper" ]
                    [ div [ class "skjemaelement" ]
                        [ Html.input
                            [ type_ "text"
                            , value options.dag
                            , options.id
                                |> Maybe.map id
                                |> Maybe.withDefault noAttribute
                            , classList
                                [ ( "skjemaelement__input", True )
                                , ( "input--fullbredde", True )
                                , ( "skjemaelement__input--harFeil"
                                  , options.feilmelding
                                        |> Maybe.map .feilPåDag
                                        |> Maybe.withDefault False
                                  )
                                ]
                            , placeholder "Dag"
                            , onInput options.onDagChange
                            ]
                            []
                        ]
                    ]
                , div [ class "DatoInputMedDag-mnd-wrapper" ]
                    [ Select.select
                        ""
                        options.onMånedChange
                        [ ( "", "Velg måned" )
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
                        |> Select.withMaybeSelected (Maybe.map Måned.tilString options.måned)
                        |> Select.withClass "DatoInput-måned"
                        |> Select.withFeilmelding
                            (case options.feilmelding of
                                Just { feilmelding, feilPåDag, feilPåMåned, feilPåÅr } ->
                                    if feilPåMåned then
                                        Just ""

                                    else
                                        Nothing

                                Nothing ->
                                    Nothing
                            )
                        |> Select.toHtml
                    ]
                , div [ class "skjemaelement" ]
                    [ div [ class "DatoInput-år-wrapper" ]
                        [ Html.input
                            [ type_ "text"
                            , value options.år
                            , classList
                                [ ( "skjemaelement__input", True )
                                , ( "input--fullbredde", True )
                                , ( "skjemaelement__input--harFeil"
                                  , options.feilmelding
                                        |> Maybe.map .feilPåÅr
                                        |> Maybe.withDefault False
                                  )
                                ]
                            , placeholder "År"
                            , onInput options.onÅrChange
                            ]
                            []
                        ]
                    ]
                ]
            , case options.feilmelding of
                Just feilmelding ->
                    div [ role "alert", ariaLive "assertive" ]
                        [ div [ class "skjemaelement__feilmelding" ]
                            [ text feilmelding.feilmelding ]
                        ]

                Nothing ->
                    text ""
            ]
        ]


withMaybeOnBlur : Maybe msg -> Input msg -> Input msg
withMaybeOnBlur maybeOnBlur input =
    case maybeOnBlur of
        Just onBlur ->
            Input.withOnBlur onBlur input

        Nothing ->
            input


noAttribute : Html.Attribute msg
noAttribute =
    classList []
