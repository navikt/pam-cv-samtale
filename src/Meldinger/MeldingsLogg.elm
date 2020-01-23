module Meldinger.MeldingsLogg exposing
    ( AntallOrdNesteOgForrigeMelding(..)
    , FerdigAnimertMeldingsLogg
    , FerdigAnimertStatus(..)
    , MeldingsGruppeViewState(..)
    , MeldingsLogg
    , ScrollAnimasjonStatus(..)
    , SpørsmålsGruppeViewState
    , SvarGruppeViewState(..)
    , alleMeldingerVises
    , antallOrdForrigeOgNesteMelding
    , avsluttScrollingTilInput
    , begynnÅViseBrukerInput
    , debugFullførAlleMeldinger
    , ferdigAnimert
    , fullførAnimasjon
    , init
    , leggTilBrukerInputSvar
    , leggTilSpørsmål
    , leggTilSvar
    , mapSpørsmålsgruppe
    , meldingsgrupper
    , registrerDimensjoner
    , scrollAnimasjonStatus
    , sisteMeldingId
    , startAnimasjon
    , startScrollingTilInput
    , startÅSkrive
    , tilMeldingsLogg
    , visBrukerInput
    )

import Browser.Dom as Dom exposing (Viewport)
import FrontendModuler.BrukerInput exposing (BrukerInput)
import List.Extra as List
import Meldinger.Melding as Melding exposing (Melding)
import Meldinger.SporsmalViewState as SpørsmålViewState exposing (SpørsmålViewState)
import Time


type MeldingsLogg msg
    = MeldingsLogg (MeldingsLoggInfo msg)


type alias MeldingsLoggInfo msg =
    { ferdigAnimert : List FerdigAnimertMeldingsGruppe
    , ikkeVist : IkkeVist msg
    }


type FerdigAnimertMeldingsGruppe
    = FerdigAnimertSpørsmålsGruppe (List FerdigAnimertMelding)
    | FerdigAnimertSvarGruppe Melding


type alias FerdigAnimertMelding =
    { melding : Melding
    , height : Int
    , width : Int
    }


type IkkeVist msg
    = AnimererBrukerinputTilSvar (BrukerInputAnimasjonInfo msg)
    | SpørsmålIkkeFerdigAnimert MeldingerIkkeFerdigAnimertInfo
    | VenterPåAtMeldingScrollingSkalBliFerdig
    | VenterPåÅScrolleTilInput
    | ScrollerTilInput { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, sisteSpørsmålHeight : Maybe Int }
    | AlleMeldingerFerdigAnimert


type alias BrukerInputAnimasjonInfo msg =
    { ventendeSpørsmål : List Melding
    , brukerInput : BrukerInput msg
    , msg : msg
    }


type alias MeldingerIkkeFerdigAnimertInfo =
    { nesteMelding : Melding
    , ikkeAnimerteMeldinger : List Melding
    , animasjonStatus : AnimasjonStatus
    , ferdigAnimerteMeldinger : List FerdigAnimertMelding
    }


type AnimasjonStatus
    = IngenAnimasjon
    | SkriveAnimasjon { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, samtaleElement : Dom.Element }
    | VenterPåÅFåRegistrertHøydeBredde
    | HarRegistrertHøyde { height : Int, width : Int, startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, samtaleElement : Dom.Element }



--- INIT ---


init : MeldingsLogg msg
init =
    MeldingsLogg
        { ferdigAnimert = []
        , ikkeVist = AlleMeldingerFerdigAnimert
        }



--- MELDINGER ---


leggTilSpørsmål : List Melding -> MeldingsLogg msg -> MeldingsLogg msg
leggTilSpørsmål nyeMeldinger (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AnimererBrukerinputTilSvar animasjonsInfo ->
            MeldingsLogg
                { meldingsLoggInfo
                    | ikkeVist =
                        AnimererBrukerinputTilSvar
                            { animasjonsInfo | ventendeSpørsmål = List.append animasjonsInfo.ventendeSpørsmål nyeMeldinger }
                }

        SpørsmålIkkeFerdigAnimert animasjonsInfo ->
            MeldingsLogg
                { meldingsLoggInfo
                    | ikkeVist =
                        SpørsmålIkkeFerdigAnimert
                            { animasjonsInfo | ikkeAnimerteMeldinger = List.append animasjonsInfo.ikkeAnimerteMeldinger nyeMeldinger }
                }

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            leggTilSpørsmålINyMeldingsgruppe nyeMeldinger meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            leggTilSpørsmålINyMeldingsgruppe nyeMeldinger meldingsLoggInfo

        ScrollerTilInput _ ->
            leggTilSpørsmålINyMeldingsgruppe nyeMeldinger meldingsLoggInfo

        AlleMeldingerFerdigAnimert ->
            case List.reverse meldingsLoggInfo.ferdigAnimert of
                last :: rest ->
                    case last of
                        FerdigAnimertSpørsmålsGruppe list ->
                            leggTilSpørsmålIFerdigAnimertMeldingsgruppe nyeMeldinger meldingsLoggInfo list (List.reverse rest)

                        FerdigAnimertSvarGruppe _ ->
                            leggTilSpørsmålINyMeldingsgruppe nyeMeldinger meldingsLoggInfo

                _ ->
                    leggTilSpørsmålINyMeldingsgruppe nyeMeldinger meldingsLoggInfo


leggTilSpørsmålINyMeldingsgruppe : List Melding -> MeldingsLoggInfo msg -> MeldingsLogg msg
leggTilSpørsmålINyMeldingsgruppe nyeMeldinger meldingsLoggInfo =
    case nyeMeldinger of
        first :: rest ->
            MeldingsLogg
                { meldingsLoggInfo
                    | ikkeVist =
                        SpørsmålIkkeFerdigAnimert
                            { nesteMelding = first
                            , ikkeAnimerteMeldinger = rest
                            , ferdigAnimerteMeldinger = []
                            , animasjonStatus = IngenAnimasjon
                            }
                }

        [] ->
            MeldingsLogg meldingsLoggInfo


leggTilSpørsmålIFerdigAnimertMeldingsgruppe : List Melding -> MeldingsLoggInfo msg -> List FerdigAnimertMelding -> List FerdigAnimertMeldingsGruppe -> MeldingsLogg msg
leggTilSpørsmålIFerdigAnimertMeldingsgruppe nyeMeldinger meldingsLoggInfo ferdigAnimerteSpørsmål ferdigAnimerteSpørsmålsgrupper =
    case nyeMeldinger of
        first :: rest ->
            MeldingsLogg
                { ferdigAnimert = ferdigAnimerteSpørsmålsgrupper
                , ikkeVist =
                    SpørsmålIkkeFerdigAnimert
                        { nesteMelding = first
                        , ikkeAnimerteMeldinger = rest
                        , ferdigAnimerteMeldinger = ferdigAnimerteSpørsmål
                        , animasjonStatus = IngenAnimasjon
                        }
                }

        [] ->
            MeldingsLogg meldingsLoggInfo


leggTilSvar : Melding -> MeldingsLogg msg -> MeldingsLogg msg
leggTilSvar nyMelding (MeldingsLogg info) =
    MeldingsLogg { info | ferdigAnimert = List.append info.ferdigAnimert [ FerdigAnimertSvarGruppe nyMelding ] }


leggTilBrukerInputSvar : MeldingsLogg msg -> msg -> BrukerInput msg -> MeldingsLogg msg
leggTilBrukerInputSvar (MeldingsLogg info) msg brukerInput =
    case info.ikkeVist of
        AnimererBrukerinputTilSvar brukerInputAnimasjonInfo ->
            MeldingsLogg info

        SpørsmålIkkeFerdigAnimert meldingerIkkeFerdigAnimertInfo ->
            MeldingsLogg info

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg info

        VenterPåÅScrolleTilInput ->
            MeldingsLogg info

        ScrollerTilInput record ->
            MeldingsLogg info

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg
                { info
                    | ikkeVist =
                        AnimererBrukerinputTilSvar
                            { ventendeSpørsmål = []
                            , brukerInput = brukerInput
                            , msg = msg
                            }
                }



--- FERDIGANIMERT ---


type FerdigAnimertStatus
    = FerdigAnimert FerdigAnimertMeldingsLogg
    | MeldingerGjenstår


type FerdigAnimertMeldingsLogg
    = FerdigAnimertMeldingsLogg (List FerdigAnimertMeldingsGruppe)


ferdigAnimert : MeldingsLogg msg -> FerdigAnimertStatus
ferdigAnimert (MeldingsLogg info) =
    case info.ikkeVist of
        AnimererBrukerinputTilSvar _ ->
            MeldingerGjenstår

        SpørsmålIkkeFerdigAnimert _ ->
            MeldingerGjenstår

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingerGjenstår

        VenterPåÅScrolleTilInput ->
            MeldingerGjenstår

        ScrollerTilInput _ ->
            MeldingerGjenstår

        AlleMeldingerFerdigAnimert ->
            FerdigAnimert (FerdigAnimertMeldingsLogg info.ferdigAnimert)


tilMeldingsLogg : FerdigAnimertMeldingsLogg -> MeldingsLogg msg
tilMeldingsLogg (FerdigAnimertMeldingsLogg ferdigAnimert_) =
    MeldingsLogg
        { ferdigAnimert = ferdigAnimert_
        , ikkeVist = AlleMeldingerFerdigAnimert
        }



--- ANIMASJON INFO ---


type AntallOrdNesteOgForrigeMelding
    = AlleMeldingerAnimert
    | FørsteMelding Int
    | FinnesEnForrigeMelding { forrige : Int, neste : Int }


antallOrdForrigeOgNesteMelding : MeldingsLogg msg -> AntallOrdNesteOgForrigeMelding
antallOrdForrigeOgNesteMelding (MeldingsLogg { ikkeVist }) =
    case ikkeVist of
        SpørsmålIkkeFerdigAnimert ikkeFerdigAnimertInfo ->
            let
                antallOrdNesteMelding =
                    Melding.antallOrd ikkeFerdigAnimertInfo.nesteMelding
            in
            case List.reverse ikkeFerdigAnimertInfo.ferdigAnimerteMeldinger of
                last :: _ ->
                    FinnesEnForrigeMelding { forrige = Melding.antallOrd last.melding, neste = antallOrdNesteMelding }

                _ ->
                    FørsteMelding antallOrdNesteMelding

        _ ->
            AlleMeldingerAnimert


sisteMeldingId : MeldingsLogg msg -> String
sisteMeldingId (MeldingsLogg info) =
    -- TODO: Er denne nødvendig?
    "test"


type ScrollAnimasjonStatus
    = IngenScrollAnimasjon
    | ScrollerInnSkriveIndikator { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, samtaleElement : Dom.Element }
    | ScrollerInnMelding { height : Int, startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, samtaleElement : Dom.Element }
    | ScrollerInnInputFelt { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, sisteSpørsmålHeight : Maybe Int }


scrollAnimasjonStatus : MeldingsLogg msg -> ScrollAnimasjonStatus
scrollAnimasjonStatus (MeldingsLogg info) =
    case info.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            IngenScrollAnimasjon

        AnimererBrukerinputTilSvar _ ->
            IngenScrollAnimasjon

        SpørsmålIkkeFerdigAnimert meldingerIkkeFerdigAnimertInfo ->
            case meldingerIkkeFerdigAnimertInfo.animasjonStatus of
                IngenAnimasjon ->
                    IngenScrollAnimasjon

                SkriveAnimasjon { startTidForScrolling, opprinneligViewport, samtaleElement } ->
                    ScrollerInnSkriveIndikator { startTidForScrolling = startTidForScrolling, opprinneligViewport = opprinneligViewport, samtaleElement = samtaleElement }

                VenterPåÅFåRegistrertHøydeBredde ->
                    IngenScrollAnimasjon

                HarRegistrertHøyde { height, width, startTidForScrolling, opprinneligViewport, samtaleElement } ->
                    ScrollerInnMelding { height = height, startTidForScrolling = startTidForScrolling, opprinneligViewport = opprinneligViewport, samtaleElement = samtaleElement }

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            IngenScrollAnimasjon

        VenterPåÅScrolleTilInput ->
            IngenScrollAnimasjon

        ScrollerTilInput { startTidForScrolling, opprinneligViewport, sisteSpørsmålHeight } ->
            ScrollerInnInputFelt { startTidForScrolling = startTidForScrolling, opprinneligViewport = opprinneligViewport, sisteSpørsmålHeight = sisteSpørsmålHeight }


alleMeldingerVises : MeldingsLogg msg -> Bool
alleMeldingerVises (MeldingsLogg info) =
    case info.ikkeVist of
        AnimererBrukerinputTilSvar _ ->
            False

        SpørsmålIkkeFerdigAnimert _ ->
            False

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            True

        VenterPåÅScrolleTilInput ->
            True

        ScrollerTilInput _ ->
            True

        AlleMeldingerFerdigAnimert ->
            True


visBrukerInput : MeldingsLogg msg -> Bool
visBrukerInput (MeldingsLogg info) =
    case info.ikkeVist of
        AnimererBrukerinputTilSvar _ ->
            False

        SpørsmålIkkeFerdigAnimert _ ->
            False

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            False

        VenterPåÅScrolleTilInput ->
            True

        ScrollerTilInput _ ->
            True

        AlleMeldingerFerdigAnimert ->
            True



--- ANIMASJON OPPDATERING ---


startÅSkrive : Time.Posix -> Viewport -> Dom.Element -> MeldingsLogg msg -> MeldingsLogg msg
startÅSkrive posix viewport samtaleElement (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        SpørsmålIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                IngenAnimasjon ->
                    MeldingsLogg
                        { meldingsLoggInfo
                            | ikkeVist = SpørsmålIkkeFerdigAnimert { animasjonsInfo | animasjonStatus = SkriveAnimasjon { startTidForScrolling = posix, opprinneligViewport = viewport, samtaleElement = samtaleElement } }
                        }

                _ ->
                    MeldingsLogg meldingsLoggInfo

        AnimererBrukerinputTilSvar _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo


startAnimasjon : MeldingsLogg msg -> MeldingsLogg msg
startAnimasjon (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo

        SpørsmålIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                SkriveAnimasjon _ ->
                    MeldingsLogg
                        { meldingsLoggInfo
                            | ikkeVist = SpørsmålIkkeFerdigAnimert { animasjonsInfo | animasjonStatus = VenterPåÅFåRegistrertHøydeBredde }
                        }

                _ ->
                    MeldingsLogg meldingsLoggInfo

        AnimererBrukerinputTilSvar _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo


registrerDimensjoner : { height : Int, width : Int, viewport : Viewport, posix : Time.Posix, samtaleElement : Dom.Element } -> MeldingsLogg msg -> MeldingsLogg msg
registrerDimensjoner { height, width, viewport, posix, samtaleElement } (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo

        AnimererBrukerinputTilSvar _ ->
            MeldingsLogg meldingsLoggInfo

        SpørsmålIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                VenterPåÅFåRegistrertHøydeBredde ->
                    MeldingsLogg
                        { meldingsLoggInfo
                            | ikkeVist =
                                SpørsmålIkkeFerdigAnimert
                                    { animasjonsInfo
                                        | animasjonStatus =
                                            HarRegistrertHøyde
                                                { height = height
                                                , width = width
                                                , startTidForScrolling = posix
                                                , opprinneligViewport = viewport
                                                , samtaleElement = samtaleElement
                                                }
                                    }
                        }

                _ ->
                    MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo


fullførAnimasjon : MeldingsLogg msg -> MeldingsLogg msg
fullførAnimasjon (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo

        SpørsmålIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                HarRegistrertHøyde { height, width } ->
                    case animasjonsInfo.ikkeAnimerteMeldinger of
                        first :: rest ->
                            MeldingsLogg
                                { meldingsLoggInfo
                                    | ikkeVist =
                                        SpørsmålIkkeFerdigAnimert
                                            { ferdigAnimerteMeldinger =
                                                List.append animasjonsInfo.ferdigAnimerteMeldinger
                                                    [ { melding = animasjonsInfo.nesteMelding
                                                      , height = height
                                                      , width = width
                                                      }
                                                    ]
                                            , nesteMelding = first
                                            , ikkeAnimerteMeldinger = rest
                                            , animasjonStatus = IngenAnimasjon
                                            }
                                }

                        [] ->
                            MeldingsLogg
                                { meldingsLoggInfo
                                    | ikkeVist = VenterPåAtMeldingScrollingSkalBliFerdig
                                    , ferdigAnimert =
                                        List.append meldingsLoggInfo.ferdigAnimert
                                            [ FerdigAnimertSpørsmålsGruppe
                                                (List.append animasjonsInfo.ferdigAnimerteMeldinger
                                                    [ { melding = animasjonsInfo.nesteMelding
                                                      , height = height
                                                      , width = width
                                                      }
                                                    ]
                                                )
                                            ]
                                }

                _ ->
                    MeldingsLogg meldingsLoggInfo

        AnimererBrukerinputTilSvar _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo


begynnÅViseBrukerInput : MeldingsLogg msg -> MeldingsLogg msg
begynnÅViseBrukerInput (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AnimererBrukerinputTilSvar _ ->
            MeldingsLogg meldingsLoggInfo

        SpørsmålIkkeFerdigAnimert _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg { meldingsLoggInfo | ikkeVist = VenterPåÅScrolleTilInput }

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg { meldingsLoggInfo | ikkeVist = VenterPåÅScrolleTilInput }


startScrollingTilInput : Time.Posix -> Viewport -> MeldingsLogg msg -> MeldingsLogg msg
startScrollingTilInput posix viewport (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AnimererBrukerinputTilSvar _ ->
            MeldingsLogg meldingsLoggInfo

        SpørsmålIkkeFerdigAnimert _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg { meldingsLoggInfo | ikkeVist = ScrollerTilInput { startTidForScrolling = posix, opprinneligViewport = viewport, sisteSpørsmålHeight = regnUtSisteSpørsmålHeight meldingsLoggInfo.ferdigAnimert } }

        ScrollerTilInput { startTidForScrolling } ->
            MeldingsLogg meldingsLoggInfo

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo


regnUtSisteSpørsmålHeight : List FerdigAnimertMeldingsGruppe -> Maybe Int
regnUtSisteSpørsmålHeight ferdigAnimerteMeldingsgrupper =
    case List.last ferdigAnimerteMeldingsgrupper of
        Just sisteMeldingsgruppe ->
            case sisteMeldingsgruppe of
                FerdigAnimertSpørsmålsGruppe meldinger ->
                    case List.last meldinger of
                        Just ferdigAnimertSpørsmål ->
                            Just ferdigAnimertSpørsmål.height

                        Nothing ->
                            Nothing

                FerdigAnimertSvarGruppe _ ->
                    Nothing

        Nothing ->
            Nothing


avsluttScrollingTilInput : MeldingsLogg msg -> MeldingsLogg msg
avsluttScrollingTilInput (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AnimererBrukerinputTilSvar _ ->
            MeldingsLogg meldingsLoggInfo

        SpørsmålIkkeFerdigAnimert _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput { startTidForScrolling } ->
            MeldingsLogg { meldingsLoggInfo | ikkeVist = AlleMeldingerFerdigAnimert }

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo


debugFullførAlleMeldinger : MeldingsLogg msg -> MeldingsLogg msg
debugFullførAlleMeldinger (MeldingsLogg info) =
    case info.ikkeVist of
        SpørsmålIkkeFerdigAnimert ikkeFerdigInfo ->
            MeldingsLogg
                { info
                    | ikkeVist = AlleMeldingerFerdigAnimert
                    , ferdigAnimert =
                        info.ferdigAnimert
                            ++ [ [ ikkeFerdigInfo.ferdigAnimerteMeldinger
                                 , [ debugTilFerdiganimert ikkeFerdigInfo.nesteMelding ]
                                 , List.map debugTilFerdiganimert ikkeFerdigInfo.ikkeAnimerteMeldinger
                                 ]
                                    |> List.concat
                                    |> FerdigAnimertSpørsmålsGruppe
                               ]
                }

        _ ->
            MeldingsLogg { info | ikkeVist = AlleMeldingerFerdigAnimert }


debugTilFerdiganimert : Melding -> FerdigAnimertMelding
debugTilFerdiganimert melding =
    { melding = melding
    , height = 0
    , width = 0
    }



--- VIEW ---


type MeldingsGruppeViewState msg
    = SpørsmålGruppe SpørsmålsGruppeViewState
    | SvarGruppe (SvarGruppeViewState msg)


type SpørsmålsGruppeViewState
    = FerdigAnimertViewState (List FerdigAnimertMelding)
    | IkkeFerdigAnimertViewState MeldingerIkkeFerdigAnimertInfo


type SvarGruppeViewState msg
    = FerdigAnimertSvar Melding
    | IkkeFerdigAnimertSvar msg (BrukerInput msg)


meldingsgrupper : MeldingsLogg msg -> List (MeldingsGruppeViewState msg)
meldingsgrupper (MeldingsLogg info) =
    case info.ikkeVist of
        AnimererBrukerinputTilSvar ikkeFerdigAnimertInfo ->
            [ List.map ferdigAnimertMeldingsGruppeTilViewState info.ferdigAnimert
            , [ SvarGruppe (IkkeFerdigAnimertSvar ikkeFerdigAnimertInfo.msg ikkeFerdigAnimertInfo.brukerInput) ]
            ]
                |> List.concat

        SpørsmålIkkeFerdigAnimert ikkeFerdigAnimertInfo ->
            [ List.map ferdigAnimertMeldingsGruppeTilViewState info.ferdigAnimert
            , ikkeFerdigAnimertInfo
                |> IkkeFerdigAnimertViewState
                |> SpørsmålGruppe
                |> List.singleton
            ]
                |> List.concat

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            List.map ferdigAnimertMeldingsGruppeTilViewState info.ferdigAnimert

        VenterPåÅScrolleTilInput ->
            List.map ferdigAnimertMeldingsGruppeTilViewState info.ferdigAnimert

        ScrollerTilInput _ ->
            List.map ferdigAnimertMeldingsGruppeTilViewState info.ferdigAnimert

        AlleMeldingerFerdigAnimert ->
            List.map ferdigAnimertMeldingsGruppeTilViewState info.ferdigAnimert


ferdigAnimertMeldingsGruppeTilViewState : FerdigAnimertMeldingsGruppe -> MeldingsGruppeViewState msg
ferdigAnimertMeldingsGruppeTilViewState meldingsgruppe =
    case meldingsgruppe of
        FerdigAnimertSpørsmålsGruppe list ->
            SpørsmålGruppe (FerdigAnimertViewState list)

        FerdigAnimertSvarGruppe melding ->
            SvarGruppe (FerdigAnimertSvar melding)


mapSpørsmålsgruppe : (SpørsmålViewState -> a) -> SpørsmålsGruppeViewState -> List a
mapSpørsmålsgruppe function spørsmålsGruppeViewState =
    case spørsmålsGruppeViewState of
        FerdigAnimertViewState ferdigAnimerteMeldinger ->
            ferdigAnimerteMeldinger
                |> ferdigAnimerteSpørsmålTilViewState
                |> List.map function

        IkkeFerdigAnimertViewState meldingerIkkeFerdigAnimertInfo ->
            ikkeFerdigAnimertTilViewState meldingerIkkeFerdigAnimertInfo.ferdigAnimerteMeldinger meldingerIkkeFerdigAnimertInfo
                |> List.map function


ikkeFerdigAnimertTilViewState : List FerdigAnimertMelding -> MeldingerIkkeFerdigAnimertInfo -> List SpørsmålViewState
ikkeFerdigAnimertTilViewState ferdigAnimerte info =
    case ferdigAnimerte of
        [] ->
            [ nesteMeldingTilViewState True info ]

        last :: [] ->
            case info.animasjonStatus of
                IngenAnimasjon ->
                    [ SpørsmålViewState.initFerdigAnimert last.melding "test1"
                    , nesteMeldingTilViewState False info
                    ]

                SkriveAnimasjon _ ->
                    [ SpørsmålViewState.initFerdigAnimertFørNyMelding { height = last.height } last.melding "test1"
                    , nesteMeldingTilViewState False info
                    ]

                VenterPåÅFåRegistrertHøydeBredde ->
                    [ SpørsmålViewState.initFerdigAnimert last.melding "test1"
                        |> SpørsmålViewState.utenIkon
                    , nesteMeldingTilViewState False info
                    ]

                HarRegistrertHøyde { height, width } ->
                    [ SpørsmålViewState.initFerdigAnimert last.melding "test1"
                        |> SpørsmålViewState.utenIkon
                    , nesteMeldingTilViewState False info
                    ]

        first :: rest ->
            (SpørsmålViewState.initFerdigAnimert first.melding "annet"
                |> SpørsmålViewState.utenIkon
            )
                :: ikkeFerdigAnimertTilViewState rest info


nesteMeldingTilViewState : Bool -> MeldingerIkkeFerdigAnimertInfo -> SpørsmålViewState
nesteMeldingTilViewState førsteMeldingIMeldingsgruppe meldingerIkkeFerdigAnimertInfo =
    case meldingerIkkeFerdigAnimertInfo.animasjonStatus of
        IngenAnimasjon ->
            SpørsmålViewState.init meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"

        SkriveAnimasjon _ ->
            if førsteMeldingIMeldingsgruppe then
                SpørsmålViewState.initSkriver meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"
                    |> SpørsmålViewState.medIkonForFørsteMelding

            else
                SpørsmålViewState.initSkriver meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"

        VenterPåÅFåRegistrertHøydeBredde ->
            SpørsmålViewState.initKalkuleres meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"

        HarRegistrertHøyde { height, width } ->
            SpørsmålViewState.initFerdigKalkulert { height = height, width = width } meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"


ferdigAnimerteSpørsmålTilViewState : List FerdigAnimertMelding -> List SpørsmålViewState
ferdigAnimerteSpørsmålTilViewState meldinger =
    List.indexedMap
        (\index { melding } ->
            if index == List.length meldinger - 1 then
                SpørsmålViewState.initFerdigAnimert melding "ferdig"

            else
                SpørsmålViewState.initFerdigAnimert melding "ferdig"
                    |> SpørsmålViewState.utenIkon
        )
        meldinger
