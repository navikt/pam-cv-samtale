module FrontendModuler.BrukerInput exposing
    ( BrukerInput
    , KnapperLayout(..)
    , brukerInputMedGåVidereKnapp
    , datoInputMedGåVidereKnapp
    , inputMedGåVidereKnapp
    , knapper
    , lenke
    , månedKnapper
    , selectMedGåVidereKnapp
    , selectMedGåVidereKnapp2
    , skjema
    , textareaMedGåVidereKnapp
    , tilSvarMelding
    , toHtml
    , typeaheadMedGåVidereKnapp
    , utenInnhold
    )

import Dato.Maned as Måned exposing (Måned)
import FrontendModuler.BrukerInputMedGaVidereKnapp as BrukerInputMedGåVidereKnapp exposing (BrukerInputMedGåVidereKnapp)
import FrontendModuler.DatoInputMedDag exposing (DatoInputMedDag)
import FrontendModuler.Input exposing (Input)
import FrontendModuler.Knapp as Knapp exposing (Knapp)
import FrontendModuler.Lenke as Lenke exposing (Lenke)
import FrontendModuler.ManedKnapper as MånedKnapper
import FrontendModuler.Select exposing (Select)
import FrontendModuler.Textarea exposing (Textarea)
import FrontendModuler.Typeahead exposing (Typeahead)
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Extra as List
import Meldinger.Melding as Melding exposing (Melding)


type BrukerInput msg
    = Knapper KnapperLayout (List (Knapp msg))
    | BrukerInputMedGåVidereKnapp (BrukerInputMedGåVidereKnapp msg)
    | MånedKnapper { onMånedValg : Måned -> msg, onAvbryt : msg }
    | Skjema { lagreMsg : msg, lagreKnappTekst : String } (List (Html msg))
    | Lenke (Lenke msg)
    | UtenInnhold



--- KONSTRUKTØRER ---


type KnapperLayout
    = Flytende
    | Kolonne
    | VarighetGrid


knapper : KnapperLayout -> List (Knapp msg) -> BrukerInput msg
knapper =
    Knapper


månedKnapper : { onMånedValg : Måned -> msg, onAvbryt : msg } -> BrukerInput msg
månedKnapper =
    MånedKnapper


inputMedGåVidereKnapp : { onGåVidere : msg, onAvbryt : msg } -> Input msg -> BrukerInput msg
inputMedGåVidereKnapp { onGåVidere, onAvbryt } inputElement =
    BrukerInputMedGåVidereKnapp.input onGåVidere inputElement
        |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp onAvbryt
        |> brukerInputMedGåVidereKnapp


brukerInputMedGåVidereKnapp : BrukerInputMedGåVidereKnapp msg -> BrukerInput msg
brukerInputMedGåVidereKnapp =
    BrukerInputMedGåVidereKnapp


textareaMedGåVidereKnapp : msg -> Textarea msg -> BrukerInput msg
textareaMedGåVidereKnapp gåVidereMsg textareaElement =
    BrukerInputMedGåVidereKnapp.textarea gåVidereMsg textareaElement
        |> brukerInputMedGåVidereKnapp


typeaheadMedGåVidereKnapp : { onGåVidere : msg, onAvbryt : msg } -> Typeahead msg -> BrukerInput msg
typeaheadMedGåVidereKnapp { onGåVidere, onAvbryt } typeaheadElement =
    BrukerInputMedGåVidereKnapp.typeahead onGåVidere typeaheadElement
        |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp onAvbryt
        |> brukerInputMedGåVidereKnapp


selectMedGåVidereKnapp : msg -> Select msg -> BrukerInput msg
selectMedGåVidereKnapp gåVidereMsg selectElement =
    BrukerInputMedGåVidereKnapp.select gåVidereMsg selectElement
        |> brukerInputMedGåVidereKnapp


selectMedGåVidereKnapp2 : { onGåVidere : msg, onAvbryt : msg } -> Select msg -> BrukerInput msg
selectMedGåVidereKnapp2 { onGåVidere, onAvbryt } selectElement =
    BrukerInputMedGåVidereKnapp.select onGåVidere selectElement
        |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp onAvbryt
        |> brukerInputMedGåVidereKnapp


datoInputMedGåVidereKnapp : { onGåVidere : msg, onAvbryt : msg } -> DatoInputMedDag msg -> BrukerInput msg
datoInputMedGåVidereKnapp { onGåVidere, onAvbryt } datoInputElement =
    BrukerInputMedGåVidereKnapp.datoInput onGåVidere datoInputElement
        |> BrukerInputMedGåVidereKnapp.withAvbrytKnapp onAvbryt
        |> brukerInputMedGåVidereKnapp


skjema : { lagreMsg : msg, lagreKnappTekst : String } -> List (Html msg) -> BrukerInput msg
skjema =
    Skjema


lenke : Lenke msg -> BrukerInput msg
lenke =
    Lenke


utenInnhold : BrukerInput msg
utenInnhold =
    UtenInnhold



--- TIL HTML ---


toHtml : BrukerInput msg -> Html msg
toHtml brukerInput =
    case brukerInput of
        Knapper layout knappeElementer ->
            case layout of
                Flytende ->
                    div [ class "knapperad" ]
                        [ div [ class "knapper--flytende" ]
                            (List.map Knapp.toHtml knappeElementer)
                        ]

                Kolonne ->
                    div [ class "knapperad" ]
                        [ div [ class "knapper--kolonne" ]
                            (List.map Knapp.toHtml knappeElementer)
                        ]

                VarighetGrid ->
                    div [ class "knapperad" ]
                        [ div [ class "knapper--varighet" ]
                            (List.map Knapp.toHtml knappeElementer)
                        ]

        Skjema { lagreMsg, lagreKnappTekst } skjemaelementer ->
            div [ class "skjema-wrapper" ]
                [ div [ class "skjema" ]
                    (List.concat
                        [ skjemaelementer
                        , [ div []
                                [ Knapp.knapp lagreMsg lagreKnappTekst
                                    |> Knapp.toHtml
                                ]
                          ]
                        ]
                    )
                ]

        Lenke lenkeElement ->
            div [ class "knapperad" ]
                [ div [ class "knapper--flytende" ]
                    [ lenkeElement
                        |> Lenke.toHtml
                    ]
                ]

        MånedKnapper { onMånedValg, onAvbryt } ->
            MånedKnapper.månedKnapper { onMånedValg = onMånedValg, onAvbryt = onAvbryt }

        UtenInnhold ->
            text ""

        BrukerInputMedGåVidereKnapp brukerInputMedGåVidereKnapp_ ->
            BrukerInputMedGåVidereKnapp.toHtml brukerInputMedGåVidereKnapp_



--- TIL MELDING ---


tilSvarMelding : msg -> BrukerInput msg -> Melding
tilSvarMelding msg brukerInput =
    tilString msg brukerInput
        |> List.singleton
        |> Melding.svar


tilString : msg -> BrukerInput msg -> String
tilString msg brukerInput =
    case brukerInput of
        Knapper _ knappeElementer ->
            knappeElementer
                |> List.find (\knapp -> Knapp.msg knapp == msg)
                |> Maybe.map Knapp.innhold
                |> Maybe.withDefault ""

        BrukerInputMedGåVidereKnapp brukerInputMedGåVidereKnapp_ ->
            BrukerInputMedGåVidereKnapp.tilString msg brukerInputMedGåVidereKnapp_

        Skjema _ _ ->
            ""

        Lenke lenke_ ->
            Lenke.tekst_ lenke_

        UtenInnhold ->
            ""

        MånedKnapper { onMånedValg, onAvbryt } ->
            if msg == onAvbryt then
                "Avbryt"

            else
                Måned.måneder
                    |> List.find (\måned -> onMånedValg måned == msg)
                    |> Maybe.map Måned.tilString
                    |> Maybe.withDefault ""
