module Seksjon.Sprak exposing
    ( EnkeltSpråk
    , Ferdighet(..)
    , Model
    , Msg
    , SamtaleStatus(..)
    , SpråkListe
    , init
    , meldingsLogg
    , nesteSamtaleSteg
    , samtaleTilMeldingsLogg
    , update
    , viewBrukerInput
    )

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
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Skjema.Sprak as SpråkSkjema exposing (SpråkSkjema)
import Sprakkoder exposing (Sprakkoder)
import Task



-- MODEL --


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
    | SpråkkodeneFeilet (Maybe Http.Error)
    | LagringFeilet EnkeltSpråk Http.Error SpråkSkjema
    | VenterPåAnimasjonFørFullføring


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig FerdigAnimertMeldingsLogg


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


norsk =
    EnkeltSpråk "Norsk" Morsmål Morsmål


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg


innlagteSpråk : Model -> SpråkListe
innlagteSpråk (Model info) =
    info.språk



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
    | BrukerVilHenteSpråkPåNytt
    | StartÅSkrive
    | FullførMelding


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        NorskErMorsmål ->
            let
                skjema =
                    SpråkSkjema.init "Norsk" "FOERSTESPRAAK" "FOERSTESPRAAK"
            in
            ( nesteSamtaleSteg model (Melding.svar [ "Ja" ]) (LagrerNorsk norsk skjema)
            , Cmd.batch
                [ leggTilSpråkAPI skjema
                , lagtTilSpørsmålCmd
                ]
            )
                |> IkkeFerdig

        NorskErIkkeMorsmål ->
            ( LeggTilFerdighetMuntlig "Norsk"
                |> nesteSamtaleSteg model (Melding.svar [ "Nei" ])
            , lagtTilSpørsmålCmd
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
             , Cmd.batch
                [ leggTilSpråkAPI skjema
                , lagtTilSpørsmålCmd
                ]
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

                            Morsmål ->
                                "Dette er mitt morsmål"
                        ]
                    )
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerKanEngelsk ->
            ( LeggTilFerdighetMuntlig "Engelsk"
                |> nesteSamtaleSteg model (Melding.svar [ "Ja" ])
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerKanIkkeEngelsk ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei" ]) (IntroLeggTilNorsk model.språk)
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerVilLeggeTilSpråk språkNavn ->
            ( LeggTilFerdighetMuntlig språkNavn
                |> nesteSamtaleSteg model (Melding.svar [ språkNavn ])
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        BrukerKanFlereSpråk ->
            case model.språkKoder of
                Success list ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (VelgNyttSpråk Nothing)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (SpråkkodeneFeilet Nothing)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd
                        ]
                    )
                        |> IkkeFerdig

        BrukerVilAvslutteSeksjonen ->
            ( nesteSamtaleSteg model (Melding.svar [ "Nei, gå videre" ]) VenterPåAnimasjonFørFullføring
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig

        ViewportSatt result ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        SpråkLagtTil result ->
            case result of
                Ok value ->
                    case model.aktivSamtale of
                        LagrerNorsk enkeltSpråk språkSkjema ->
                            ( nesteSamtaleStegUtenMelding { model | språk = value } LeggTilEngelsk
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        LagrerAndre enkeltSpråk språkSkjema ->
                            ( nesteSamtaleStegUtenMelding { model | språk = value } (LeggTilFlereSpråk enkeltSpråk (Model model))
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        LagringFeilet enkeltSpråk error språkskjema ->
                            ( nesteSamtaleSteg { model | språk = value } (Melding.svar [ "Ja, prøv på nytt" ]) (LeggTilFlereSpråk enkeltSpråk (Model model))
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        _ ->
                            ( Model model
                            , Cmd.none
                            )
                                |> IkkeFerdig

                Err error ->
                    case model.aktivSamtale of
                        LagrerNorsk enkeltSpråk språkSkjema ->
                            ( nesteSamtaleSteg model (Melding.svar [ "Legg til norsk" ]) (LagringFeilet enkeltSpråk error språkSkjema)
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        LagrerAndre enkeltSpråk språkSkjema ->
                            ( nesteSamtaleSteg model (Melding.svar [ "Legg til " ++ String.toLower enkeltSpråk.språkNavn ]) (LagringFeilet enkeltSpråk error språkSkjema)
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        _ ->
                            ( Model model
                            , Cmd.batch
                                [ logFeilmelding error "Lagre språk"
                                ]
                            )
                                |> IkkeFerdig

        ErrorLogget result ->
            ( Model model, Cmd.none ) |> IkkeFerdig

        SpråkkoderHentet result ->
            case result of
                Ok koder ->
                    ( Model { model | språkKoder = Success koder }
                    , lagtTilSpørsmålCmd
                    )
                        |> IkkeFerdig

                Err error ->
                    ( Model model
                    , Cmd.batch
                        [ logFeilmelding error "Språkkoder Hentet"
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
                            , lagtTilSpørsmålCmd
                            )
                                |> IkkeFerdig

                        Nothing ->
                            ( Model model, Cmd.none ) |> IkkeFerdig

                _ ->
                    ( Model model, Cmd.none ) |> IkkeFerdig

        SendSkjemaPåNytt språkSkjema nyModel ->
            ( nyModel, leggTilSpråkAPI språkSkjema ) |> IkkeFerdig

        BrukerVilHenteSpråkPåNytt ->
            case model.språkKoder of
                Success list ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, legg til språk" ]) (VelgNyttSpråk Nothing)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd
                        ]
                    )
                        |> IkkeFerdig

                _ ->
                    ( nesteSamtaleSteg model (Melding.svar [ "Ja, prøve på nytt" ]) (SpråkkodeneFeilet Nothing)
                    , Cmd.batch
                        [ lagtTilSpørsmålCmd
                        , hentSpråkkoder
                        ]
                    )
                        |> IkkeFerdig

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , Process.sleep (MeldingsLogg.nesteMeldingToString model.seksjonsMeldingsLogg * 1000.0)
                    |> Task.perform (\_ -> FullførMelding)
                ]
            )
                |> IkkeFerdig

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring ->
                    Ferdig ferdigAnimertSamtale

                _ ->
                    ( Model
                        { model
                            | seksjonsMeldingsLogg =
                                nyMeldingsLogg
                        }
                    , SamtaleAnimasjon.scrollTilBunn ViewportSatt
                    )
                        |> IkkeFerdig

        MeldingerGjenstår ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        nyMeldingsLogg
                }
            , lagtTilSpørsmålCmd
            )
                |> IkkeFerdig


fullførSeksjonHvisMeldingsloggErFerdig : ModelInfo -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig modelInfo =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring }, Cmd.none )
                |> IkkeFerdig


lagtTilSpørsmålCmd : Cmd Msg
lagtTilSpørsmålCmd =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , Process.sleep 200
            |> Task.perform (\_ -> StartÅSkrive)
        ]


logFeilmelding : Http.Error -> String -> Cmd Msg
logFeilmelding error operasjon =
    Feilmelding.feilmelding operasjon error
        |> Maybe.map (Api.logError ErrorLogget)
        |> Maybe.withDefault Cmd.none


leggTilSpråkAPI : SpråkSkjema -> Cmd Msg
leggTilSpråkAPI skjema =
    Api.postSpråk SpråkLagtTil skjema


hentSpråkkoder : Cmd Msg
hentSpråkkoder =
    Api.getSpråkkoder SpråkkoderHentet


listOppSpråkFraSpråkliste : SpråkListe -> String
listOppSpråkFraSpråkliste språkListe =
    String.concat
        (List.map
            (\el ->
                if List.elemIndex el språkListe == Just 0 then
                    Spraakferdighet.sprak el |> Maybe.withDefault ""

                else if List.elemIndex el språkListe == Just (List.length språkListe - 1) then
                    String.concat [ " og ", String.toLower (Spraakferdighet.sprak el |> Maybe.withDefault "") ]

                else
                    String.concat [ ", ", String.toLower (Spraakferdighet.sprak el |> Maybe.withDefault "") ]
            )
            språkListe
        )


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg språkSeksjon =
    case språkSeksjon of
        IntroLeggTilNorsk språkListe ->
            if List.isEmpty språkListe then
                [ Melding.spørsmål [ "Nå skal vi legge inn språk." ]
                , Melding.spørsmål [ "La oss begynne med norsk. Er norsk førstespråket (morsmålet) ditt?" ]
                ]

            else
                [ Melding.spørsmål [ "Nå skal vi legge til språk." ]
                , Melding.spørsmål
                    [ "Jeg ser at du har lagt inn disse språkene allerede:"
                    , listOppSpråkFraSpråkliste språkListe
                    ]
                , Melding.spørsmål [ "Vil du legge til flere?" ]
                ]

        LeggTilEngelsk ->
            [ Melding.spørsmål [ "Hva med engelsk? Kan du det?" ]
            ]

        LeggTilFlereSpråk enkeltSpråk model ->
            [ Melding.spørsmål
                [ "Supert! Da har jeg lagt til " ++ String.toLower enkeltSpråk.språkNavn ++ "."
                , duHarNåLagtInnTilString enkeltSpråk model
                , "Kan du flere språk?"
                ]
            ]

        LeggTilFerdighetSkriftlig enkeltSpråk ->
            [ Melding.spørsmål [ "Hvor godt skriver du " ++ String.toLower enkeltSpråk.språkNavn ++ "?" ] ]

        LeggTilFerdighetMuntlig enkeltSpråk ->
            [ Melding.spørsmål [ "Hvor godt snakker du " ++ String.toLower enkeltSpråk ++ "?" ] ]

        LagringFeilet enkeltSpråk error språkSkjema ->
            [ Melding.spørsmål [ "Oops... Jeg klarte ikke å lagre " ++ String.toLower enkeltSpråk.språkNavn ++ ".", "Vil du prøve på nytt?" ] ]

        VelgNyttSpråk _ ->
            [ Melding.spørsmål [ "Hvilket språk vil du legge til?" ] ]

        LagrerNorsk enkeltSpråk språkSkjema ->
            []

        LagrerAndre enkeltSpråk språkSkjema ->
            []

        SpråkkodeneFeilet error ->
            [ Melding.spørsmål [ "Oops! Noe gikk galt...", "Vil du prøve på nytt eller avslutte og gå videre?", "Prøv gjerne igjen senere." ] ]

        VenterPåAnimasjonFørFullføring ->
            [ Melding.spørsmål [ "Da var vi ferdige med språkdelen." ] ]


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


nesteSamtaleStegUtenMelding : ModelInfo -> Samtale -> Model
nesteSamtaleStegUtenMelding model samtaleSeksjon =
    Model
        { model
            | aktivSamtale = samtaleSeksjon
            , seksjonsMeldingsLogg =
                model.seksjonsMeldingsLogg
                    |> MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg samtaleSeksjon)
        }


duHarNåLagtInnTilString : EnkeltSpråk -> Model -> String
duHarNåLagtInnTilString enkeltSpråk model =
    "Du har nå lagt inn "
        ++ String.concat
            (List.map
                (\el ->
                    String.toLower (Spraakferdighet.sprak el |> Maybe.withDefault "")
                        ++ (if List.elemIndex el (innlagteSpråk model) == Just (List.length (innlagteSpråk model) - 1) then
                                " "

                            else
                                ", "
                           )
                )
                (innlagteSpråk model)
                ++ [ "og ", String.toLower enkeltSpråk.språkNavn ]
            )



-- VIEW --


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                IntroLeggTilNorsk språkListe ->
                    if List.isEmpty språkListe then
                        div [ class "skjema-wrapper" ]
                            [ div [ class "skjema" ]
                                [ div [ class "inputrad" ]
                                    [ Knapp.knapp NorskErMorsmål "Ja"
                                        |> Knapp.toHtml
                                    , Knapp.knapp NorskErIkkeMorsmål "Nei"
                                        |> Knapp.toHtml
                                    ]
                                ]
                            ]

                    else
                        div [ class "skjema-wrapper" ]
                            [ div [ class "skjema" ]
                                [ div [ class "inputkolonne" ]
                                    [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                                        |> Knapp.toHtml
                                    , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                        |> Knapp.toHtml
                                    ]
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
                    div [ class "skjema-wrapper" ]
                        [ div [ class "skjema" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp BrukerKanFlereSpråk "Ja, legg til språk"
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, gå videre"
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                LeggTilFerdighetSkriftlig enkeltSpråk ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp (SkriftligNivå (EnkeltSpråk enkeltSpråk.språkNavn enkeltSpråk.muntlig Nybegynner)) "Jeg er nybegynner"
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ Knapp.knapp (SkriftligNivå (EnkeltSpråk enkeltSpråk.språkNavn enkeltSpråk.muntlig Godt)) ("Jeg skriver godt " ++ String.toLower enkeltSpråk.språkNavn)
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ Knapp.knapp (SkriftligNivå (EnkeltSpråk enkeltSpråk.språkNavn enkeltSpråk.muntlig VeldigGodt)) ("Jeg skriver veldig godt " ++ String.toLower enkeltSpråk.språkNavn)
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                LeggTilFerdighetMuntlig enkeltSpråk ->
                    div [ class "skjema-wrapper" ]
                        [ div [ class "knapperad-wrapper" ]
                            [ div [ class "inputkolonne" ]
                                [ Knapp.knapp (MuntligNivå (SpråkMedMuntlig enkeltSpråk Nybegynner)) "Jeg er nybegynner"
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ Knapp.knapp (MuntligNivå (SpråkMedMuntlig enkeltSpråk Godt)) ("Jeg snakker godt " ++ String.toLower enkeltSpråk)
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            , div [ class "inputkolonne" ]
                                [ Knapp.knapp (MuntligNivå (SpråkMedMuntlig enkeltSpråk VeldigGodt)) ("Jeg snakker veldig godt " ++ String.toLower enkeltSpråk)
                                    |> Knapp.withClass Knapp.SpråknivåKnapp
                                    |> Knapp.toHtml
                                ]
                            ]
                        ]

                VelgNyttSpråk valgtSpråk ->
                    case model.språkKoder of
                        Success list ->
                            div [ class "skjema-wrapper" ]
                                [ div [ class "skjema" ]
                                    [ div [ class "inputkolonne" ]
                                        [ div []
                                            [ Select.select "Språk"
                                                ValgtSpråk
                                                (( "Velg språk", "Velg språk" )
                                                    :: List.map
                                                        (\el ->
                                                            ( Sprakkoder.kode el, Sprakkoder.term el )
                                                        )
                                                        list
                                                )
                                                |> Select.toHtml
                                            , Knapp.knapp BrukerHarValgtSpråk "Legg til"
                                                |> Knapp.withEnabled
                                                    (if valgtSpråk /= Nothing then
                                                        Knapp.Enabled

                                                     else
                                                        Knapp.Disabled
                                                    )
                                                |> Knapp.withClass Knapp.SpråknivåKnapp
                                                |> Knapp.toHtml
                                            ]
                                        ]
                                    ]
                                ]

                        Loading ->
                            div [ class "inputkolonne" ]
                                [ div [ class "inputkolonne-innhold" ]
                                    [ text "Loading..." ]
                                ]

                        Failure error ->
                            div [ class "inputkolonne" ]
                                [ div [ class "inputkolonne-innhold" ]
                                    [ text "Noe gikk galt..." ]
                                ]

                LagringFeilet enkeltSpråk error failedSpråkSkjema ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
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

                SpråkkodeneFeilet error ->
                    div [ class "inputkolonne" ]
                        [ div [ class "inputkolonne-innhold" ]
                            [ Knapp.knapp BrukerVilHenteSpråkPåNytt "Ja, prøv på nytt"
                                |> Knapp.toHtml
                            , Knapp.knapp BrukerVilAvslutteSeksjonen "Nei, avslutt og gå videre"
                                |> Knapp.toHtml
                            ]
                        ]

                VenterPåAnimasjonFørFullføring ->
                    text ""

        MeldingerGjenstår ->
            text ""



-- INIT --


init : FerdigAnimertMeldingsLogg -> List Spraakferdighet -> ( Model, Cmd Msg )
init gammelMeldingsLogg språkFerdighet =
    let
        aktivSamtale =
            IntroLeggTilNorsk språkFerdighet
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , språk = språkFerdighet
        , språkKoder = Loading
        }
    , Cmd.batch
        [ lagtTilSpørsmålCmd
        , hentSpråkkoder
        ]
    )
