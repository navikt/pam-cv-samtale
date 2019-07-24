module Seksjon.Sprak exposing (EnkeltSpråk, Ferdighet(..), Model(..), ModelInfo, Msg, Samtale(..), SamtaleStatus(..), SpråkListe, init, meldingsLogg, nesteSamtaleSteg, samtaleTilMeldingsLogg, update, viewBrukerInput)

-- MODEL --

import Api
import Browser.Dom as Dom
import Cv.Spraakferdighet as Spraakferdighet exposing (Spraakferdighet)
import Feilmelding
import FrontendModuler.Input as Input
import FrontendModuler.Knapp as Knapp
import FrontendModuler.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List.Extra as List
import Melding exposing (Melding(..))
import MeldingsLogg exposing (MeldingsLogg)
import SamtaleAnimasjon
import Skjema.Sprak as SpråkSkjema exposing (SpråkSkjema)
import Sprakkoder exposing (Sprakkoder)


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , språk : SpråkListe
    , språkKoder : RemoteDataSpråkKoder
    }


type Samtale
    = IntroLeggTilNorsk SpråkListe
    | LagrerNorsk EnkeltSpråk SpråkSkjema
    | LeggTilEngelsk
    | LeggTilFlereSpråk EnkeltSpråk Model
    | VelgNyttSpråk (Maybe Sprakkoder)
    | LeggTilFerdighetMuntlig String
    | LeggTilFerdighetSkriftlig SpråkMedMuntlig
    | LagrerAndre EnkeltSpråk SpråkSkjema
    | LagringFeilet EnkeltSpråk Http.Error SpråkSkjema


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig MeldingsLogg


type RemoteDataSpråkKoder
    = Loading
    | Success (List Sprakkoder)
    | Failure Http.Error


type alias SpråkListe =
    List Spraakferdighet


type alias SpråkMedMuntlig =
    { språkNavn : String
    , muntlig : Ferdighet
    }


type alias EnkeltSpråk =
    { språkNavn : String
    , ferdighetMuntlig : Ferdighet
    , ferdighetSkriftlig : Ferdighet
    }


type Ferdighet
    = Nybegynner
    | Godt
    | VeldigGodt
    | Morsmål



{--
        IKKE_OPPGITT("Ikke oppgitt"),
        NYBEGYNNER("Nybegynner"),
        GODT("Godt"),
        VELDIG_GODT("Veldig godt"),
        FOERSTESPRAAK("Førstespråk (morsmål)");
--}


ferdighetTilString : Ferdighet -> String
ferdighetTilString ferdighet =
    case ferdighet of
        Nybegynner ->
            "NYBEGYNNER"

        Godt ->
            "GODT"

        VeldigGodt ->
            "VELDIG_GODT"

        Morsmål ->
            "FOERSTESPRAAK"


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



-- UPDATE --


type Msg
    = ValgtSpråk String
    | BrukerHarValgtSpråk
    | NorskErMorsmål
    | NorskErIkkeMorsmål
    | BrukerKanEngelsk
    | BrukerKanIkkeEngelsk
    | BrukerKanFlereSpråk
    | MuntligNivå SpråkMedMuntlig
    | SkriftligNivå EnkeltSpråk
    | BrukerVilLeggeTilSpråk String
    | BrukerVilAvslutteSeksjonen
    | ViewportSatt (Result Dom.Error ())
    | SpråkLagtTil (Result Http.Error (List Spraakferdighet))
    | ErrorLogget (Result Http.Error ())
    | SpråkkoderHentet (Result Http.Error (List Sprakkoder))
    | SendSkjemaPåNytt SpråkSkjema Model


norsk =
    EnkeltSpråk "Norsk" Morsmål Morsmål


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        NorskErMorsmål ->
            let
                skjema =
                    SpråkSkjema.init "Norsk" "FOERSTESPRAAK" "FOERSTESPRAAK"
            in
            ( nesteSamtaleSteg model (Melding.svar [ "Ja" ]) (LagrerNorsk norsk skjema)
            , skjema
                |> leggTilSpråkAPI
            )
                |> IkkeFerdig

        NorskErIkkeMorsmål ->
            ( LeggTilFerdighetMuntlig "Norsk"
                |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
            )
                |> IkkeFerdig

        SkriftligNivå enkeltSpråk ->
            (let
                skjema =
                    SpråkSkjema.init enkeltSpråk.språkNavn (ferdighetTilString enkeltSpråk.ferdighetMuntlig) (ferdighetTilString enkeltSpråk.ferdighetSkriftlig)
             in
             ( LagrerAndre enkeltSpråk skjema
                |> nesteSamtaleSteg model
                    (Melding.svar
                        [ case enkeltSpråk.ferdighetSkriftlig of
                            Nybegynner ->
                                "Jeg er nybegynner"

                            Godt ->
                                "Jeg skriver godt " ++ String.toLower enkeltSpråk.språkNavn

                            VeldigGodt ->
                                "Jeg skriver veldig godt " ++ String.toLower enkeltSpråk.språkNavn

                            _ ->
                                "Noe gikk galt"
                        ]
                    )
             , skjema
                |> leggTilSpråkAPI
             )
            )
                |> IkkeFerdig

        MuntligNivå språkMedMuntlig ->
            ( LeggTilFerdighetSkriftlig språkMedMuntlig
                |> nesteSamtaleSteg model
                    (Melding.svar
                        [ case språkMedMuntlig.muntlig of
                            Nybegynner ->
                                "Jeg er nybegynner"

                            Godt ->
                                "Jeg snakker godt " ++ String.toLower språkMedMuntlig.språkNavn

                            VeldigGodt ->
                                "Jeg snakker veldig godt " ++ String.toLower språkMedMuntlig.språkNavn

                            _ ->
                                "Noe gikk galt"
                        ]
                    )
            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
            )
                |> IkkeFerdig

        BrukerKanEngelsk ->
            ( LeggTilFerdighetMuntlig "Engelsk"
                |> nesteSamtaleSteg model (Melding.svar [ "Ja" ])
            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
            )
                |> IkkeFerdig

        BrukerKanIkkeEngelsk ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei" ]) (IntroLeggTilNorsk model.språk)
            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
            )
                |> IkkeFerdig

        BrukerVilLeggeTilSpråk språkNavn ->
            ( LeggTilFerdighetMuntlig språkNavn
                |> nesteSamtaleSteg model (Melding.svar [ språkNavn ])
            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
            )
                |> IkkeFerdig

        BrukerKanFlereSpråk ->
            ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (VelgNyttSpråk Nothing)
            , case model.språkKoder of
                Success list ->
                    Cmd.batch
                        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        ]

                _ ->
                    Cmd.batch
                        [ hentSpråkkoder
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        ]
            )
                |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.leggTilSvar (Melding.svar [ "Nei, gå videre" ])
                |> MeldingsLogg.leggTilSpørsmål [ Melding.spørsmål [ "Okai! Da er vi ferdige med språk!" ] ]
                |> Ferdig

        ViewportSatt result ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        SpråkLagtTil result ->
            case result of
                Ok value ->
                    case model.aktivSamtale of
                        LagrerNorsk enkeltSpråk språkSkjema ->
                            ( nesteSamtaleSteg { model | språk = value } (Melding.svar [ "Norsk lagret" ]) LeggTilEngelsk
                            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                            )
                                |> IkkeFerdig

                        LagrerAndre enkeltSpråk språkSkjema ->
                            ( nesteSamtaleSteg { model | språk = value } (Melding.svar [ "Andre Språk lagret" ]) (LeggTilFlereSpråk enkeltSpråk (Model model))
                            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                            )
                                |> IkkeFerdig

                        LagringFeilet enkeltSpråk error språkskjema ->
                            ( nesteSamtaleSteg { model | språk = value } (Melding.svar [ "Ja, prøv på nytt" ]) (LeggTilFlereSpråk enkeltSpråk (Model model))
                            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                            )
                                |> IkkeFerdig

                        _ ->
                            ( Model model
                            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                            )
                                |> IkkeFerdig

                Err error ->
                    case model.aktivSamtale of
                        LagrerNorsk enkeltSpråk språkSkjema ->
                            ( nesteSamtaleSteg model (Melding.svar [ "Legg til" ]) (LagringFeilet enkeltSpråk error språkSkjema)
                            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                            )
                                |> IkkeFerdig

                        LagrerAndre enkeltSpråk språkSkjema ->
                            ( nesteSamtaleSteg model (Melding.svar [ "Legg til" ]) (LagringFeilet enkeltSpråk error språkSkjema)
                            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                            )
                                |> IkkeFerdig

                        _ ->
                            ( Model model
                            , Cmd.batch
                                [ logFeilmelding error "Lagre språk"
                                , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                                ]
                            )
                                |> IkkeFerdig

        ErrorLogget result ->
            ( Model model, Cmd.none ) |> IkkeFerdig

        SpråkkoderHentet result ->
            case result of
                Ok koder ->
                    ( nesteSamtaleSteg { model | språkKoder = Success koder } (Melding.spørsmål [ "Hvilket språk vil du legge til?" ]) (VelgNyttSpråk Nothing)
                    , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                    )
                        |> IkkeFerdig

                Err error ->
                    ( nesteSamtaleSteg model (Melding.spørsmål [ "Oops! Noe gikk galt, la oss prøve igjen..." ]) (IntroLeggTilNorsk model.språk)
                    , Cmd.batch
                        [ logFeilmelding error "Språkkoder Hentet"
                        , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                        ]
                    )
                        |> IkkeFerdig

        ValgtSpråk valgtSpråk ->
            case model.språkKoder of
                Loading ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

                Success list ->
                    ( Model { model | aktivSamtale = VelgNyttSpråk (List.find (\sprakkode -> valgtSpråk == Sprakkoder.kode sprakkode) list) }
                    , Cmd.none
                    )
                        |> IkkeFerdig

                Failure error ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        BrukerHarValgtSpråk ->
            case model.aktivSamtale of
                VelgNyttSpråk språkkode ->
                    case språkkode of
                        Just kode ->
                            ( LeggTilFerdighetMuntlig (Sprakkoder.kode kode)
                                |> nesteSamtaleSteg model (Melding.svar [ Sprakkoder.kode kode ])
                            , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( Model model, Cmd.none ) |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        SendSkjemaPåNytt språkSkjema nyModel ->
            ( nyModel, leggTilSpråkAPI språkSkjema ) |> IkkeFerdig


loadSpråkkoder : ModelInfo -> Model
loadSpråkkoder model =
    case model.språkKoder of
        Success list ->
            Model
                { seksjonsMeldingsLogg = model.seksjonsMeldingsLogg
                , aktivSamtale = model.aktivSamtale
                , språk = model.språk
                , språkKoder = Success list
                }

        _ ->
            loadSpråkkoder model


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


leggTilSpråk : ModelInfo -> SpråkListe -> ModelInfo
leggTilSpråk info liste =
    { info | språk = info.språk ++ liste }


leggTilSpråkAPI : SpråkSkjema -> Cmd Msg
leggTilSpråkAPI skjema =
    Api.leggTilSpråk SpråkLagtTil skjema


hentSpråkkoder : Cmd Msg
hentSpråkkoder =
    Api.hentSpråkkoder SpråkkoderHentet


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg språkSeksjon =
    case språkSeksjon of
        IntroLeggTilNorsk språkListe ->
            if List.isEmpty språkListe then
                [ Melding.spørsmål [ "Nå skal vi legge inn språk." ]
                , Melding.spørsmål [ "La oss begynne med norsk. Er norsk førstespråket (morsmålet) ditt?" ]
                ]

            else
                [ Melding.spørsmål
                    [ "Jeg ser at du har lagt inn disse språkene allerede:"
                    , String.concat (List.map (\el -> " " ++ (Spraakferdighet.sprak el |> Maybe.withDefault "")) språkListe)
                    ]
                , Melding.spørsmål [ "Vil du legge til flere?" ]
                ]

        LeggTilEngelsk ->
            [ Melding.spørsmål [ "Hva med engelsk? Kan du det?" ]
            ]

        LeggTilFlereSpråk enkeltSpråk model ->
            [ Melding.spørsmål
                [ "Supert! Da har du lagt inn "
                    ++ String.concat (List.map (\el -> String.toLower (Spraakferdighet.sprak el |> Maybe.withDefault "") ++ ", ") (innlagteSpråk model) ++ [ "og ", enkeltSpråk.språkNavn ])
                , "Kan du flere språk?"
                ]
            ]

        LeggTilFerdighetSkriftlig enkeltSpråk ->
            [ Melding.spørsmål [ "Hvor godt skriver du " ++ enkeltSpråk.språkNavn ++ "?" ] ]

        LeggTilFerdighetMuntlig enkeltSpråk ->
            [ Melding.spørsmål [ "Hvor godt snakker du " ++ enkeltSpråk ++ "?" ] ]

        VelgNyttSpråk _ ->
            []

        LagringFeilet enkeltSpråk error språkSkjema ->
            [ Melding.spørsmål [ "Oops... Noe gikk galt!", "Vil du prøve på nytt?" ] ]

        LagrerNorsk enkeltSpråk språkSkjema ->
            []

        LagrerAndre enkeltSpråk språkSkjema ->
            []


innlagteSpråk : Model -> SpråkListe
innlagteSpråk (Model info) =
    info.språk


nesteSamtaleSteg : ModelInfo -> Melding -> Samtale -> Model
nesteSamtaleSteg model melding samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSvar melding
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }



-- VIEW --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case model.aktivSamtale of
        IntroLeggTilNorsk språkListe ->
            if List.isEmpty språkListe then
                div [ class "inputrad" ]
                    [ div [ class "inputrad-innhold" ]
                        [ Knapp.knapp NorskErMorsmål "Ja"
                            |> Knapp.toHtml
                        , Knapp.knapp NorskErIkkeMorsmål "Nei"
                            |> Knapp.toHtml
                        ]
                    ]

            else
                div [ class "inputrad" ]
                    [ div [ class "inputrad-innhold" ]
                        [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                            |> Knapp.toHtml
                        , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                            |> Knapp.toHtml
                        ]
                    ]

        LeggTilEngelsk ->
            div [ class "inputrad" ]
                [ div [ class "inputrad-innhold" ]
                    [ Knapp.knapp BrukerKanEngelsk "Ja"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerKanIkkeEngelsk "Nei"
                        |> Knapp.toHtml
                    ]
                ]

        LeggTilFlereSpråk enkeltSpråk språkListe ->
            div [ class "inputrad" ]
                [ div [ class "inputrad-innhold" ]
                    [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                        |> Knapp.toHtml
                    ]
                ]

        LeggTilFerdighetSkriftlig enkeltSpråk ->
            div [ class "inputrad" ]
                [ div [ class "inputrad-innhold" ]
                    [ div [ class "skjema" ]
                        [ Knapp.knapp (SkriftligNivå (EnkeltSpråk enkeltSpråk.språkNavn enkeltSpråk.muntlig Nybegynner)) "Jeg er nybegynner"
                            |> Knapp.toHtml
                        , Knapp.knapp (SkriftligNivå (EnkeltSpråk enkeltSpråk.språkNavn enkeltSpråk.muntlig Godt)) ("Jeg skriver godt " ++ String.toLower enkeltSpråk.språkNavn)
                            |> Knapp.toHtml
                        , Knapp.knapp (SkriftligNivå (EnkeltSpråk enkeltSpråk.språkNavn enkeltSpråk.muntlig VeldigGodt)) ("Jeg skriver veldig godt " ++ String.toLower enkeltSpråk.språkNavn)
                            |> Knapp.toHtml
                        ]
                    ]
                ]

        LeggTilFerdighetMuntlig enkeltSpråk ->
            div [ class "inputrad" ]
                [ div [ class "inputrad-innhold" ]
                    [ div [ class "skjema" ]
                        [ Knapp.knapp (MuntligNivå (SpråkMedMuntlig enkeltSpråk Nybegynner)) "Jeg er nybegynner"
                            |> Knapp.toHtml
                        , Knapp.knapp (MuntligNivå (SpråkMedMuntlig enkeltSpråk Godt)) ("Jeg snakker godt " ++ String.toLower enkeltSpråk)
                            |> Knapp.toHtml
                        , Knapp.knapp (MuntligNivå (SpråkMedMuntlig enkeltSpråk VeldigGodt)) ("Jeg snakker veldig godt " ++ String.toLower enkeltSpråk)
                            |> Knapp.toHtml
                        ]
                    ]
                ]

        VelgNyttSpråk valgtSpråk ->
            case model.språkKoder of
                Success list ->
                    div []
                        [ div [ class "inputrad" ]
                            [ div [ class "inputrad-innhold" ]
                                [ Select.select "Språk" ValgtSpråk (( "Velg språk", "Velg språk" ) :: List.map (\el -> ( Sprakkoder.kode el, Sprakkoder.term el )) list) |> Select.toHtml
                                ]
                            ]
                        , div [ class "inputrad" ]
                            [ Knapp.knapp BrukerHarValgtSpråk "Legg til"
                                |> Knapp.withEnabled
                                    (if valgtSpråk /= Nothing then
                                        Knapp.Enabled

                                     else
                                        Knapp.Disabled
                                    )
                                |> Knapp.toHtml
                            ]
                        ]

                Loading ->
                    div [ class "inputrad" ]
                        [ div [ class "inputrad-innhold" ]
                            [ text "Loading..." ]
                        ]

                Failure error ->
                    div [ class "inputrad" ]
                        [ div [ class "inputrad-innhold" ]
                            [ text "Noe gikk galt..." ]
                        ]

        LagringFeilet enkeltSpråk error failedSpråkSkjema ->
            div [ class "inputrad" ]
                [ div [ class "inputrad-innhold" ]
                    [ Knapp.knapp (SendSkjemaPåNytt failedSpråkSkjema (Model model)) "Ja, prøv på nytt"
                        |> Knapp.toHtml
                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                        |> Knapp.toHtml
                    ]
                ]

        LagrerNorsk enkeltSpråk språkSkjema ->
            text ""

        LagrerAndre enkeltSpråk språkskjema ->
            text ""



-- INIT --


init : MeldingsLogg -> List Spraakferdighet -> Model
init gammelMeldingsLogg språkFerdighet =
    let
        aktivSamtale =
            IntroLeggTilNorsk språkFerdighet
    in
    Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) gammelMeldingsLogg
        , aktivSamtale = aktivSamtale
        , språk = språkFerdighet
        , språkKoder = Loading
        }
