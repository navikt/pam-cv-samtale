module Seksjon.Seksjonsvalg exposing
    ( Model
    , Msg
    , SamtaleStatus(..)
    , Seksjonsvalg(..)
    , initLeggTilAutorisasjoner
    , initLeggTilFlereAnnet
    , initLeggTilFlereAutorisasjoner
    , meldingsLogg
    , update
    , viewBrukerInput
    )

import Browser.Dom as Dom
import DebugStatus exposing (DebugStatus)
import FrontendModuler.Containers as Containers exposing (KnapperLayout(..))
import FrontendModuler.Knapp as Knapp exposing (Enabled(..))
import Html exposing (..)
import Melding exposing (Melding)
import MeldingsLogg exposing (FerdigAnimertMeldingsLogg, FerdigAnimertStatus(..), MeldingsLogg, tilMeldingsLogg)
import Process
import SamtaleAnimasjon
import Task



--- MODEL ---


type Model
    = Model ModelInfo


type alias ModelInfo =
    { seksjonsMeldingsLogg : MeldingsLogg
    , aktivSamtale : Samtale
    , debugStatus : DebugStatus
    }


type Seksjonsvalg
    = FagbrevSvennebrevSeksjon
    | MesterbrevSeksjon
    | AutorisasjonSeksjon
    | SertifiseringSeksjon
    | AnnenErfaringSeksjon
    | KursSeksjon
    | FørerkortSeksjon
    | IngenAvSeksjonene


type Samtale
    = LeggTilAutorisasjoner
    | LeggTilFlereAutorisasjoner
    | LeggTilAnnet
    | LeggTilFlereAnnet
    | VenterPåAnimasjonFørFullføring Seksjonsvalg


type SamtaleStatus
    = IkkeFerdig ( Model, Cmd Msg )
    | Ferdig Seksjonsvalg FerdigAnimertMeldingsLogg


meldingsLogg : Model -> MeldingsLogg
meldingsLogg (Model model) =
    model.seksjonsMeldingsLogg



--- UPDATE ---


type Msg
    = StartÅSkrive
    | FullførMelding
    | ViewportSatt (Result Dom.Error ())
    | SeksjonValgt Seksjonsvalg
    | BrukerVilGåTilNesteDel String


update : Msg -> Model -> SamtaleStatus
update msg (Model model) =
    case msg of
        ViewportSatt _ ->
            ( Model model, Cmd.none )
                |> IkkeFerdig

        StartÅSkrive ->
            ( Model
                { model
                    | seksjonsMeldingsLogg =
                        MeldingsLogg.startÅSkrive model.seksjonsMeldingsLogg
                }
            , Cmd.batch
                [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
                , (MeldingsLogg.nesteMeldingToString model.seksjonsMeldingsLogg * 1000.0)
                    |> DebugStatus.meldingsTimeout model.debugStatus
                    |> Process.sleep
                    |> Task.perform (always FullførMelding)
                ]
            )
                |> IkkeFerdig

        FullførMelding ->
            model.seksjonsMeldingsLogg
                |> MeldingsLogg.fullførMelding
                |> updateEtterFullførtMelding model

        SeksjonValgt seksjon ->
            fullførSeksjonHvisMeldingsloggErFerdig seksjon
                { model
                    | seksjonsMeldingsLogg =
                        model.seksjonsMeldingsLogg
                            |> MeldingsLogg.leggTilSvar (Melding.svar [ seksjonsvalgTilString seksjon ])
                }

        BrukerVilGåTilNesteDel knappeTekst ->
            ( nesteSamtaleSteg model (Melding.svar [ knappeTekst ]) LeggTilAnnet
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig


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


fullførSeksjonHvisMeldingsloggErFerdig : Seksjonsvalg -> ModelInfo -> SamtaleStatus
fullførSeksjonHvisMeldingsloggErFerdig seksjon modelInfo =
    case MeldingsLogg.ferdigAnimert modelInfo.seksjonsMeldingsLogg of
        FerdigAnimert ferdigAnimertMeldingsLogg ->
            Ferdig seksjon ferdigAnimertMeldingsLogg

        MeldingerGjenstår ->
            ( Model { modelInfo | aktivSamtale = VenterPåAnimasjonFørFullføring seksjon }, Cmd.none )
                |> IkkeFerdig


updateEtterFullførtMelding : ModelInfo -> MeldingsLogg -> SamtaleStatus
updateEtterFullførtMelding model nyMeldingsLogg =
    case MeldingsLogg.ferdigAnimert nyMeldingsLogg of
        FerdigAnimert ferdigAnimertSamtale ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring seksjon ->
                    Ferdig seksjon ferdigAnimertSamtale

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
            , lagtTilSpørsmålCmd model.debugStatus
            )
                |> IkkeFerdig


lagtTilSpørsmålCmd : DebugStatus -> Cmd Msg
lagtTilSpørsmålCmd debugStatus =
    Cmd.batch
        [ SamtaleAnimasjon.scrollTilBunn ViewportSatt
        , 200
            |> DebugStatus.meldingsTimeout debugStatus
            |> Process.sleep
            |> Task.perform (always StartÅSkrive)
        ]


samtaleTilMeldingsLogg : Samtale -> List Melding
samtaleTilMeldingsLogg avslutningsSeksjon =
    case avslutningsSeksjon of
        VenterPåAnimasjonFørFullføring _ ->
            []

        LeggTilAutorisasjoner ->
            [ Melding.spørsmål [ "Nå begynner CV-en din å ta form. Er det noe mer du kan legge inn?" ]
            , Melding.spørsmål [ "Vil du legge til noen av disse kategoriene?" ]
            ]

        LeggTilFlereAutorisasjoner ->
            [ Melding.spørsmål [ "Vil du legge til flere kategorier?" ] ]

        LeggTilAnnet ->
            [ Melding.spørsmål [ "Det er viktig å få med alt du kan på CV-en." ]
            , Melding.spørsmål [ "Har du jobbet som frivillig eller har hatt verv? Legg til annen erfaring." ]
            , Melding.spørsmål [ "Har du tatt norskprøve? Legg til kurs." ]
            , Melding.spørsmål [ "Hva med førerkort? Husk å legge det inn i CV-en din. Mange arbeidsgivere ser etter jobbsøkere som kan kjøre." ]
            , Melding.spørsmål [ "Vil du legge til noen av disse kategoriene?" ]
            ]

        LeggTilFlereAnnet ->
            [ Melding.spørsmål [ "Vil du legge til flere kategorier?" ] ]


--- VIEW ---


viewBrukerInput : Model -> Html Msg
viewBrukerInput (Model model) =
    case MeldingsLogg.ferdigAnimert model.seksjonsMeldingsLogg of
        FerdigAnimert _ ->
            case model.aktivSamtale of
                VenterPåAnimasjonFørFullføring _ ->
                    text ""

                LeggTilAutorisasjoner ->
                    viewLeggTilAutorisasjoner

                LeggTilFlereAutorisasjoner ->
                    viewLeggTilAutorisasjoner

                LeggTilAnnet ->
                    viewLeggTilAnnet

                LeggTilFlereAnnet ->
                    viewLeggTilAnnet

        MeldingerGjenstår ->
            text ""


viewLeggTilAutorisasjoner : Html Msg
viewLeggTilAutorisasjoner =
    Containers.knapper Kolonne
        [ seksjonsvalgKnapp FagbrevSvennebrevSeksjon
        , seksjonsvalgKnapp MesterbrevSeksjon
        , seksjonsvalgKnapp AutorisasjonSeksjon
        , seksjonsvalgKnapp SertifiseringSeksjon
        , Knapp.knapp (BrukerVilGåTilNesteDel "Nei, gå videre") "Nei, gå videre"
            |> Knapp.toHtml
        ]


viewLeggTilAnnet : Html Msg
viewLeggTilAnnet =
    Containers.knapper Kolonne
        [ seksjonsvalgKnapp AnnenErfaringSeksjon
        , seksjonsvalgKnapp KursSeksjon
        , seksjonsvalgKnapp FørerkortSeksjon
        , seksjonsvalgKnapp IngenAvSeksjonene
        ]


seksjonsvalgKnapp : Seksjonsvalg -> Html Msg
seksjonsvalgKnapp seksjonsvalg =
    seksjonsvalg
        |> seksjonsvalgTilString
        |> Knapp.knapp (SeksjonValgt seksjonsvalg)
        |> Knapp.withEnabled (seksjonsvalgDisabled seksjonsvalg)
        |> Knapp.toHtml


seksjonsvalgDisabled : Seksjonsvalg -> Enabled
seksjonsvalgDisabled seksjonsvalg =
    -- TODO: enable når implementert
    -- TODO: Slett denne når alle er implementert
    case seksjonsvalg of
        FagbrevSvennebrevSeksjon ->
            Enabled

        MesterbrevSeksjon ->
            Enabled

        AutorisasjonSeksjon ->
            Enabled

        SertifiseringSeksjon ->
            Enabled

        AnnenErfaringSeksjon ->
            Enabled

        KursSeksjon ->
            Disabled

        FørerkortSeksjon ->
            Disabled

        IngenAvSeksjonene ->
            Enabled


seksjonsvalgTilString : Seksjonsvalg -> String
seksjonsvalgTilString seksjonsvalg =
    case seksjonsvalg of
        FagbrevSvennebrevSeksjon ->
            "Fagbrev/Svennebrev"

        MesterbrevSeksjon ->
            "Mesterbrev"

        AutorisasjonSeksjon ->
            "Autorisasjon"

        SertifiseringSeksjon ->
            "Sertifisering/sertifikat"

        AnnenErfaringSeksjon ->
            "Annen erfaring"

        KursSeksjon ->
            "Kurs"

        FørerkortSeksjon ->
            "Førerkort"

        IngenAvSeksjonene ->
            "Nei, gå videre"



--- INIT ---


init : Samtale -> DebugStatus -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
init aktivSamtale_ debugStatus gammelMeldingsLogg =
    let
        aktivSamtale =
            aktivSamtale_
    in
    ( Model
        { seksjonsMeldingsLogg =
            MeldingsLogg.leggTilSpørsmål (samtaleTilMeldingsLogg aktivSamtale) (tilMeldingsLogg gammelMeldingsLogg)
        , aktivSamtale = aktivSamtale
        , debugStatus = debugStatus
        }
    , lagtTilSpørsmålCmd debugStatus
    )


initLeggTilFlereAutorisasjoner : DebugStatus -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
initLeggTilFlereAutorisasjoner =
    init LeggTilFlereAutorisasjoner


initLeggTilFlereAnnet : DebugStatus -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
initLeggTilFlereAnnet =
    init LeggTilFlereAnnet


initLeggTilAutorisasjoner : DebugStatus -> FerdigAnimertMeldingsLogg -> ( Model, Cmd Msg )
initLeggTilAutorisasjoner =
    init LeggTilAutorisasjoner
