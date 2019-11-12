module MeldingsLogg exposing
    (  FerdigAnimertMeldingsLogg
       --    , MeldingsGruppe(..)

    , FerdigAnimertStatus(..)
    , IkonView(..)
    , MeldingsGruppeViewState(..)
    ,  MeldingsLogg
       --    ,  MeldingsPlassering(..)
       --    , SkriveStatus(..)
       --    , ferdigAnimert
       --    , fullførMelding

    , ScrollAnimasjonStatus(..)
    , SpørsmålsGruppeViewState
    , avsluttScrollingTilInput
    , begynnÅViseBrukerInput
    , ferdigAnimert
    , fullførAnimasjon
    , ikonPlassering
    , init
    , leggTilSpørsmål
    ,  leggTilSvar
       --    , mapMeldingsGruppe
       --    , mapMeldingsGruppeMeldinger
       --    , nesteMeldingToString
       --    , skriveStatus

    , mapMeldingsGruppe
    , mapSpørsmålsgruppe
    , nesteMeldingToString
    , registrerDimensjoner
    , scrollAnimasjonStatus
    , sisteMeldingId
    , startAnimasjon
    , startScrollingTilInput
    ,  startÅSkrive
       --    , tilMeldingsLogg

    , tilMeldingsLogg
    , visBrukerInput
    )

import Browser.Dom exposing (Viewport)
import List.Extra as List
import Melding exposing (Melding)
import SporsmalViewState as SpørsmålViewState exposing (SpørsmålViewState)
import Time


type MeldingsLogg
    = MeldingsLogg MeldingsLoggInfo


type alias MeldingsLoggInfo =
    { ferdigAnimert : List FerdigAnimertMeldingsGruppe
    , ikkeVist : IkkeVist
    }


type FerdigAnimertMeldingsGruppe
    = FerdigAnimertSpørsmålsGruppe (List FerdigAnimertMelding)
    | FerdigAnimertSvarGruppe Melding


type alias FerdigAnimertMelding =
    { melding : Melding
    , height : Int
    , width : Int
    }


type IkkeVist
    = MeldingerIkkeFerdigAnimert MeldingerIkkeFerdigAnimertInfo
    | VenterPåAtMeldingScrollingSkalBliFerdig
    | VenterPåÅScrolleTilInput
    | ScrollerTilInput { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport }
    | AlleMeldingerFerdigAnimert


type alias MeldingerIkkeFerdigAnimertInfo =
    { nesteMelding : Melding
    , ikkeAnimerteMeldinger : List Melding
    , animasjonStatus : AnimasjonStatus
    , ferdigAnimerteMeldinger : List FerdigAnimertMelding
    }


type AnimasjonStatus
    = IngenAnimasjon
    | SkriveAnimasjon { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport }
    | VenterPåÅFåRegistrertHøydeBredde
    | HarRegistrertHøyde { height : Int, width : Int, startTidForScrolling : Time.Posix, opprinneligViewport : Viewport }



--- INIT ---


init : MeldingsLogg
init =
    MeldingsLogg
        { ferdigAnimert = []
        , ikkeVist = AlleMeldingerFerdigAnimert
        }



--- OPPDATERING ---


nesteMeldingToString : MeldingsLogg -> Float
nesteMeldingToString (MeldingsLogg { ikkeVist }) =
    -- TODO: Erstatt denne funksjonene med noe annet
    2.0


sisteMeldingId : MeldingsLogg -> String
sisteMeldingId (MeldingsLogg info) =
    -- TODO: Er denne nødvendig?
    "test"


type ScrollAnimasjonStatus
    = IngenScrollAnimasjon
    | ScrollerInnSkriveIndikator { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport }
    | ScrollerInnMelding { height : Int, startTidForScrolling : Time.Posix, opprinneligViewport : Viewport }
    | ScrollerInnInputFelt { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport }


scrollAnimasjonStatus : MeldingsLogg -> ScrollAnimasjonStatus
scrollAnimasjonStatus (MeldingsLogg info) =
    case info.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            IngenScrollAnimasjon

        MeldingerIkkeFerdigAnimert meldingerIkkeFerdigAnimertInfo ->
            case meldingerIkkeFerdigAnimertInfo.animasjonStatus of
                IngenAnimasjon ->
                    IngenScrollAnimasjon

                SkriveAnimasjon { startTidForScrolling, opprinneligViewport } ->
                    ScrollerInnSkriveIndikator { startTidForScrolling = startTidForScrolling, opprinneligViewport = opprinneligViewport }

                VenterPåÅFåRegistrertHøydeBredde ->
                    IngenScrollAnimasjon

                HarRegistrertHøyde { height, width, startTidForScrolling, opprinneligViewport } ->
                    ScrollerInnMelding { height = height, startTidForScrolling = startTidForScrolling, opprinneligViewport = opprinneligViewport }

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            IngenScrollAnimasjon

        VenterPåÅScrolleTilInput ->
            IngenScrollAnimasjon

        ScrollerTilInput { startTidForScrolling, opprinneligViewport } ->
            ScrollerInnInputFelt { startTidForScrolling = startTidForScrolling, opprinneligViewport = opprinneligViewport }


visBrukerInput : MeldingsLogg -> Bool
visBrukerInput (MeldingsLogg info) =
    -- TODO Endre til at ikkeVist blir satt til ScrollerTilInput etter siste melding
    case info.ikkeVist of
        MeldingerIkkeFerdigAnimert meldingerIkkeFerdigAnimertInfo ->
            False

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            False

        VenterPåÅScrolleTilInput ->
            True

        ScrollerTilInput _ ->
            True

        AlleMeldingerFerdigAnimert ->
            True



--- MELDINGER ---


leggTilSpørsmål : List Melding -> MeldingsLogg -> MeldingsLogg
leggTilSpørsmål nyeMeldinger (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        MeldingerIkkeFerdigAnimert animasjonsInfo ->
            MeldingsLogg
                { meldingsLoggInfo
                    | ikkeVist =
                        MeldingerIkkeFerdigAnimert
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


leggTilSpørsmålINyMeldingsgruppe : List Melding -> MeldingsLoggInfo -> MeldingsLogg
leggTilSpørsmålINyMeldingsgruppe nyeMeldinger meldingsLoggInfo =
    case nyeMeldinger of
        first :: rest ->
            MeldingsLogg
                { meldingsLoggInfo
                    | ikkeVist =
                        MeldingerIkkeFerdigAnimert
                            { nesteMelding = first
                            , ikkeAnimerteMeldinger = rest
                            , ferdigAnimerteMeldinger = []
                            , animasjonStatus = IngenAnimasjon
                            }
                }

        [] ->
            MeldingsLogg meldingsLoggInfo


leggTilSpørsmålIFerdigAnimertMeldingsgruppe : List Melding -> MeldingsLoggInfo -> List FerdigAnimertMelding -> List FerdigAnimertMeldingsGruppe -> MeldingsLogg
leggTilSpørsmålIFerdigAnimertMeldingsgruppe nyeMeldinger meldingsLoggInfo ferdigAnimerteSpørsmål ferdigAnimerteSpørsmålsgrupper =
    case nyeMeldinger of
        first :: rest ->
            MeldingsLogg
                { ferdigAnimert = ferdigAnimerteSpørsmålsgrupper
                , ikkeVist =
                    MeldingerIkkeFerdigAnimert
                        { nesteMelding = first
                        , ikkeAnimerteMeldinger = rest
                        , ferdigAnimerteMeldinger = ferdigAnimerteSpørsmål
                        , animasjonStatus = IngenAnimasjon
                        }
                }

        [] ->
            MeldingsLogg meldingsLoggInfo


leggTilSvar : Melding -> MeldingsLogg -> MeldingsLogg
leggTilSvar nyMelding (MeldingsLogg info) =
    MeldingsLogg { info | ferdigAnimert = List.append info.ferdigAnimert [ FerdigAnimertSvarGruppe nyMelding ] }



--- ANIMASJON ---


startÅSkrive : Time.Posix -> Viewport -> MeldingsLogg -> MeldingsLogg
startÅSkrive posix viewport (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        MeldingerIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                IngenAnimasjon ->
                    MeldingsLogg
                        { meldingsLoggInfo
                            | ikkeVist = MeldingerIkkeFerdigAnimert { animasjonsInfo | animasjonStatus = SkriveAnimasjon { startTidForScrolling = posix, opprinneligViewport = viewport } }
                        }

                _ ->
                    MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo


startAnimasjon : MeldingsLogg -> MeldingsLogg
startAnimasjon (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo

        MeldingerIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                SkriveAnimasjon _ ->
                    MeldingsLogg
                        { meldingsLoggInfo
                            | ikkeVist = MeldingerIkkeFerdigAnimert { animasjonsInfo | animasjonStatus = VenterPåÅFåRegistrertHøydeBredde }
                        }

                _ ->
                    MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo


registrerDimensjoner : { height : Int, width : Int, viewport : Viewport, posix : Time.Posix } -> MeldingsLogg -> MeldingsLogg
registrerDimensjoner { height, width, viewport, posix } (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo

        MeldingerIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                VenterPåÅFåRegistrertHøydeBredde ->
                    MeldingsLogg
                        { meldingsLoggInfo
                            | ikkeVist =
                                MeldingerIkkeFerdigAnimert
                                    { animasjonsInfo
                                        | animasjonStatus =
                                            HarRegistrertHøyde
                                                { height = height
                                                , width = width
                                                , startTidForScrolling = posix
                                                , opprinneligViewport = viewport
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


fullførAnimasjon : MeldingsLogg -> MeldingsLogg
fullførAnimasjon (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo

        MeldingerIkkeFerdigAnimert animasjonsInfo ->
            case animasjonsInfo.animasjonStatus of
                HarRegistrertHøyde { height, width } ->
                    case animasjonsInfo.ikkeAnimerteMeldinger of
                        first :: rest ->
                            MeldingsLogg
                                { meldingsLoggInfo
                                    | ikkeVist =
                                        MeldingerIkkeFerdigAnimert
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

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo


begynnÅViseBrukerInput : MeldingsLogg -> MeldingsLogg
begynnÅViseBrukerInput (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        MeldingerIkkeFerdigAnimert _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg { meldingsLoggInfo | ikkeVist = VenterPåÅScrolleTilInput }

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput _ ->
            MeldingsLogg meldingsLoggInfo

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo


startScrollingTilInput : Time.Posix -> Viewport -> MeldingsLogg -> MeldingsLogg
startScrollingTilInput posix viewport (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        MeldingerIkkeFerdigAnimert _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg { meldingsLoggInfo | ikkeVist = ScrollerTilInput { startTidForScrolling = posix, opprinneligViewport = viewport } }

        ScrollerTilInput { startTidForScrolling } ->
            MeldingsLogg meldingsLoggInfo

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo


avsluttScrollingTilInput : MeldingsLogg -> MeldingsLogg
avsluttScrollingTilInput (MeldingsLogg meldingsLoggInfo) =
    case meldingsLoggInfo.ikkeVist of
        MeldingerIkkeFerdigAnimert _ ->
            MeldingsLogg meldingsLoggInfo

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            MeldingsLogg meldingsLoggInfo

        VenterPåÅScrolleTilInput ->
            MeldingsLogg meldingsLoggInfo

        ScrollerTilInput { startTidForScrolling } ->
            MeldingsLogg { meldingsLoggInfo | ikkeVist = AlleMeldingerFerdigAnimert }

        AlleMeldingerFerdigAnimert ->
            MeldingsLogg meldingsLoggInfo



--- FERDIGANIMERT ---


type FerdigAnimertStatus
    = FerdigAnimert FerdigAnimertMeldingsLogg
    | MeldingerGjenstår


type FerdigAnimertMeldingsLogg
    = FerdigAnimertMeldingsLogg (List FerdigAnimertMeldingsGruppe)


ferdigAnimert : MeldingsLogg -> FerdigAnimertStatus
ferdigAnimert (MeldingsLogg info) =
    case info.ikkeVist of
        MeldingerIkkeFerdigAnimert _ ->
            MeldingerGjenstår

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            -- TODO: Skal denne kanskje ikke være Ferdig
            FerdigAnimert (FerdigAnimertMeldingsLogg info.ferdigAnimert)

        VenterPåÅScrolleTilInput ->
            -- TODO: Skal denne kanskje ikke være Ferdig
            FerdigAnimert (FerdigAnimertMeldingsLogg info.ferdigAnimert)

        ScrollerTilInput _ ->
            -- TODO: Skal denne kanskje ikke være Ferdig
            FerdigAnimert (FerdigAnimertMeldingsLogg info.ferdigAnimert)

        AlleMeldingerFerdigAnimert ->
            FerdigAnimert (FerdigAnimertMeldingsLogg info.ferdigAnimert)


tilMeldingsLogg : FerdigAnimertMeldingsLogg -> MeldingsLogg
tilMeldingsLogg (FerdigAnimertMeldingsLogg ferdigAnimert_) =
    MeldingsLogg
        { ferdigAnimert = ferdigAnimert_
        , ikkeVist = AlleMeldingerFerdigAnimert
        }



--- VIEW ---


type MeldingsGruppeViewState
    = SpørsmålGruppe SpørsmålsGruppeViewState
    | SvarGruppe Melding


type SpørsmålsGruppeViewState
    = FerdigAnimertViewState (List FerdigAnimertMelding)
    | IkkeFerdigAnimertViewState MeldingerIkkeFerdigAnimertInfo


mapMeldingsGruppe : (MeldingsGruppeViewState -> a) -> MeldingsLogg -> List a
mapMeldingsGruppe function (MeldingsLogg info) =
    case info.ikkeVist of
        MeldingerIkkeFerdigAnimert ikkeFerdigAnimertInfo ->
            [ List.map ferdigAnimertMeldingsGruppeTilViewState info.ferdigAnimert
            , ikkeFerdigAnimertInfo
                |> IkkeFerdigAnimertViewState
                |> SpørsmålGruppe
                |> List.singleton
            ]
                |> List.concat
                |> List.map function

        VenterPåAtMeldingScrollingSkalBliFerdig ->
            info.ferdigAnimert
                |> List.map ferdigAnimertMeldingsGruppeTilViewState
                |> List.map function

        VenterPåÅScrolleTilInput ->
            info.ferdigAnimert
                |> List.map ferdigAnimertMeldingsGruppeTilViewState
                |> List.map function

        ScrollerTilInput _ ->
            info.ferdigAnimert
                |> List.map ferdigAnimertMeldingsGruppeTilViewState
                |> List.map function

        AlleMeldingerFerdigAnimert ->
            info.ferdigAnimert
                |> List.map ferdigAnimertMeldingsGruppeTilViewState
                |> List.map function


ferdigAnimertMeldingsGruppeTilViewState : FerdigAnimertMeldingsGruppe -> MeldingsGruppeViewState
ferdigAnimertMeldingsGruppeTilViewState meldingsgruppe =
    case meldingsgruppe of
        FerdigAnimertSpørsmålsGruppe list ->
            SpørsmålGruppe (FerdigAnimertViewState list)

        FerdigAnimertSvarGruppe melding ->
            SvarGruppe melding


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



--            [ meldingerIkkeFerdigAnimertInfo.ferdigAnimerteMeldinger
--                |> List.map ferdigAnimertSpørsmålTilViewState
--            , case meldingerIkkeFerdigAnimertInfo.animasjonStatus of
--                IngenAnimasjon ->
--                    [ SpørsmålViewState.init meldingerIkkeFerdigAnimertInfo.nesteMelding ]
--
--                SkriveAnimasjon ->
--                    [ SpørsmålViewState.initSkriver meldingerIkkeFerdigAnimertInfo.nesteMelding ]
--
--                VenterPåÅFåRegistrertHøydeBredde ->
--                    [ SpørsmålViewState.initKalkuleres meldingerIkkeFerdigAnimertInfo.nesteMelding ]
--
--                HarRegistrertHøyde { height, width } ->
--                    [ SpørsmålViewState.initFerdigKalkulert { height = height, width = width } meldingerIkkeFerdigAnimertInfo.nesteMelding ]
--            ]
--                |> List.concat
--                |> List.map function


ikkeFerdigAnimertTilViewState : List FerdigAnimertMelding -> MeldingerIkkeFerdigAnimertInfo -> List SpørsmålViewState
ikkeFerdigAnimertTilViewState ferdigAnimerte info =
    case ferdigAnimerte of
        [] ->
            [ nesteMeldingTilViewState info ]

        last :: [] ->
            case info.animasjonStatus of
                IngenAnimasjon ->
                    [ SpørsmålViewState.initFerdigAnimert last.melding "test1"
                    , nesteMeldingTilViewState info
                    ]

                SkriveAnimasjon _ ->
                    [ SpørsmålViewState.initFerdigAnimertFørNyMelding { height = last.height } last.melding "test1"
                    , nesteMeldingTilViewState info
                    ]

                VenterPåÅFåRegistrertHøydeBredde ->
                    [ SpørsmålViewState.initFerdigAnimert last.melding "test1"
                        |> SpørsmålViewState.utenIkon
                    , nesteMeldingTilViewState info
                    ]

                HarRegistrertHøyde { height, width } ->
                    [ SpørsmålViewState.initFerdigAnimert last.melding "test1"
                        |> SpørsmålViewState.utenIkon
                    , nesteMeldingTilViewState info
                    ]

        first :: rest ->
            (SpørsmålViewState.initFerdigAnimert first.melding "annet"
                |> SpørsmålViewState.utenIkon
            )
                :: ikkeFerdigAnimertTilViewState rest info


nesteMeldingTilViewState : MeldingerIkkeFerdigAnimertInfo -> SpørsmålViewState
nesteMeldingTilViewState meldingerIkkeFerdigAnimertInfo =
    case meldingerIkkeFerdigAnimertInfo.animasjonStatus of
        IngenAnimasjon ->
            SpørsmålViewState.init meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"

        SkriveAnimasjon _ ->
            SpørsmålViewState.initSkriver meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"

        VenterPåÅFåRegistrertHøydeBredde ->
            SpørsmålViewState.initKalkuleres meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"

        HarRegistrertHøyde { height, width } ->
            SpørsmålViewState.initFerdigKalkulert { height = height, width = width } meldingerIkkeFerdigAnimertInfo.nesteMelding "test0"



-- TODO : Endre dette:


type IkonView
    = IkonSkjult
    | UtenPadding
    | MedPadding { height : Int }


ikonPlassering : SpørsmålsGruppeViewState -> IkonView
ikonPlassering spørsmålsGruppeViewState =
    case spørsmålsGruppeViewState of
        FerdigAnimertViewState ferdigAnimerteMeldinger ->
            UtenPadding

        IkkeFerdigAnimertViewState meldingerIkkeFerdigAnimertInfo ->
            case meldingerIkkeFerdigAnimertInfo.animasjonStatus of
                IngenAnimasjon ->
                    IkonSkjult

                SkriveAnimasjon _ ->
                    UtenPadding

                VenterPåÅFåRegistrertHøydeBredde ->
                    UtenPadding

                HarRegistrertHøyde { height, width } ->
                    MedPadding { height = height }


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



--type MeldingsPlassering
--    = SisteSpørsmålIMeldingsgruppe
--    | IkkeSisteSpørsmål
--
--
--type MeldingsGruppePlassering
--    = SisteSpørsmålsgruppe
--    | AnnenGruppe
--
--mapMeldingsGruppe : (MeldingsGruppe -> a) -> MeldingsLogg -> List a
--mapMeldingsGruppe function ((MeldingsLogg { meldingsLogg, sisteMeldingsGruppe }) as meldingsLogg_) =
--    sisteMeldingsGruppe
--        |> Maybe.map (fraMeldingsGruppeIntern SisteSpørsmålsgruppe (skriveStatus meldingsLogg_) >> List.singleton)
--        |> Maybe.withDefault []
--        |> List.append (List.map (fraMeldingsGruppeIntern AnnenGruppe (skriveStatus meldingsLogg_)) meldingsLogg)
--        |> List.map function
--
--
--fraMeldingsGruppeIntern : MeldingsGruppePlassering -> SkriveStatus -> MeldingsGruppeIntern -> MeldingsGruppe
--fraMeldingsGruppeIntern meldingsGruppePlassering skriver meldingsGruppeIntern =
--    case meldingsGruppeIntern of
--        SpørsmålGruppe_ list ->
--            case ( meldingsGruppePlassering, skriver ) of
--                ( SisteSpørsmålsgruppe, Skriver ) ->
--                    { meldingsLogg = list
--                    , sisteMelding = Nothing
--                    }
--                        |> MeldingsGruppeMeldinger
--                        |> SpørsmålGruppe
--
--                _ ->
--                    list
--                        |> tilMeldingsGruppeMeldinger
--                        |> SpørsmålGruppe
--
--        SvarGruppe_ list ->
--            { meldingsLogg = list
--            , sisteMelding = Nothing
--            }
--                |> MeldingsGruppeMeldinger
--                |> SvarGruppe
--tilMeldingsGruppeMeldinger : List Melding -> MeldingsGruppeMeldinger
--tilMeldingsGruppeMeldinger list =
--    case List.reverse list of
--        last :: rest ->
--            { meldingsLogg = List.reverse rest
--            , sisteMelding = Just last
--            }
--                |> MeldingsGruppeMeldinger
--
--        [] ->
--            { meldingsLogg = []
--            , sisteMelding = Nothing
--            }
--                |> MeldingsGruppeMeldinger
--
--
--type MeldingsGruppeMeldinger
--    = MeldingsGruppeMeldinger
--        { meldingsLogg : List Melding
--        , sisteMelding : Maybe Melding
--        }
--
--
--mapMeldingsGruppeMeldinger : (MeldingsPlassering -> Melding -> a) -> MeldingsGruppeMeldinger -> List a
--mapMeldingsGruppeMeldinger function (MeldingsGruppeMeldinger { meldingsLogg, sisteMelding }) =
--    sisteMelding
--        |> Maybe.map (function SisteSpørsmålIMeldingsgruppe >> List.singleton)
--        |> Maybe.withDefault []
--        |> List.append (List.map (function IkkeSisteSpørsmål) meldingsLogg)
--
--
--nesteMeldingToString : MeldingsLogg -> Float
--nesteMeldingToString (MeldingsLogg { ikkeVist }) =
--    case ikkeVist of
--        Ingen ->
--            1.0
--
--        SkriverEnMelding melding list ->
--            let
--                venteTid =
--                    String.toFloat (String.fromInt (List.length (Melding.innhold melding))) |> Maybe.withDefault 1.0
--            in
--            if venteTid > 3.0 then
--                3.0
--
--            else
--                venteTid
--
--        LeserMelding melding list ->
--            let
--                venteTid =
--                    String.toFloat (String.fromInt (List.length (Melding.innhold melding))) |> Maybe.withDefault 1.0
--            in
--            if venteTid > 3.0 then
--                3.0
--
--            else
--                venteTid
--
--
--skriveStatus : MeldingsLogg -> SkriveStatus
--skriveStatus (MeldingsLogg { ikkeVist }) =
--    case ikkeVist of
--        Ingen ->
--            SkriverIkke
--
--        SkriverEnMelding _ _ ->
--            Skriver
--
--        LeserMelding _ _ ->
--            SkriverIkke
--
--
--leggTilSpørsmål : List Melding -> MeldingsLogg -> MeldingsLogg
--leggTilSpørsmål nyeMeldinger (MeldingsLogg info) =
--    MeldingsLogg
--        { info
--            | ikkeVist =
--                case nyeMeldinger of
--                    first :: rest ->
--                        case info.ikkeVist of
--                            Ingen ->
--                                LeserMelding first rest
--
--                            SkriverEnMelding melding list ->
--                                SkriverEnMelding melding (list ++ nyeMeldinger)
--
--                            LeserMelding melding list ->
--                                LeserMelding melding (list ++ nyeMeldinger)
--
--                    [] ->
--                        Ingen
--        }
--
--
--leggTilSvar : Melding -> MeldingsLogg -> MeldingsLogg
--leggTilSvar nyMelding (MeldingsLogg info) =
--    case info.sisteMeldingsGruppe of
--        Just (SpørsmålGruppe_ meldingsgruppe) ->
--            MeldingsLogg
--                { info
--                    | meldingsLogg = info.meldingsLogg ++ [ SpørsmålGruppe_ meldingsgruppe ]
--                    , sisteMeldingsGruppe = Just (SvarGruppe_ [ nyMelding ])
--                }
--
--        Just (SvarGruppe_ meldingsgruppe) ->
--            MeldingsLogg { info | sisteMeldingsGruppe = Just (SvarGruppe_ (meldingsgruppe ++ [ nyMelding ])) }
--
--        Nothing ->
--            MeldingsLogg { info | sisteMeldingsGruppe = Just (SvarGruppe_ [ nyMelding ]) }
--
--
--startÅSkrive : MeldingsLogg -> MeldingsLogg
--startÅSkrive (MeldingsLogg info) =
--    MeldingsLogg
--        { info
--            | ikkeVist =
--                case info.ikkeVist of
--                    Ingen ->
--                        Ingen
--
--                    SkriverEnMelding melding list ->
--                        SkriverEnMelding melding list
--
--                    LeserMelding melding list ->
--                        SkriverEnMelding melding list
--        }
--
--
--fullførMelding : MeldingsLogg -> MeldingsLogg
--fullførMelding (MeldingsLogg info) =
--    case info.ikkeVist of
--        Ingen ->
--            MeldingsLogg info
--
--        SkriverEnMelding melding list ->
--            case info.sisteMeldingsGruppe of
--                Just (SpørsmålGruppe_ meldingsgruppe) ->
--                    MeldingsLogg
--                        { info
--                            | sisteMeldingsGruppe = Just (SpørsmålGruppe_ (meldingsgruppe ++ [ melding ]))
--                            , ikkeVist = ikkeVistEtterFullførtMelding list
--                        }
--
--                Just (SvarGruppe_ meldingsgruppe) ->
--                    MeldingsLogg
--                        { info
--                            | meldingsLogg = info.meldingsLogg ++ [ SvarGruppe_ meldingsgruppe ]
--                            , sisteMeldingsGruppe = Just (SpørsmålGruppe_ [ melding ])
--                            , ikkeVist = ikkeVistEtterFullførtMelding list
--                        }
--
--                Nothing ->
--                    MeldingsLogg { info | sisteMeldingsGruppe = Just (SpørsmålGruppe_ [ melding ]), ikkeVist = ikkeVistEtterFullførtMelding list }
--
--        LeserMelding melding list ->
--            MeldingsLogg info
--
--
--ikkeVistEtterFullførtMelding : List Melding -> UskrevneMeldinger
--ikkeVistEtterFullførtMelding list =
--    case list of
--        first :: rest ->
--            LeserMelding first rest
--
--        [] ->
--            Ingen
--
--
--type FerdigAnimertStatus
--    = FerdigAnimert FerdigAnimertMeldingsLogg
--    | MeldingerGjenstår
--
--
--ferdigAnimert : MeldingsLogg -> FerdigAnimertStatus
--ferdigAnimert (MeldingsLogg info) =
--    case info.ikkeVist of
--        Ingen ->
--            FerdigAnimert (FerdigAnimertMeldingsLogg info.sisteMeldingsGruppe info.meldingsLogg)
--
--        SkriverEnMelding _ _ ->
--            MeldingerGjenstår
--
--        LeserMelding _ _ ->
--            MeldingerGjenstår
--
--
--tilMeldingsLogg : FerdigAnimertMeldingsLogg -> MeldingsLogg
--tilMeldingsLogg (FerdigAnimertMeldingsLogg sisteMeldingsGruppe meldingslogg) =
--    MeldingsLogg
--        { meldingsLogg = meldingslogg
--        , sisteMeldingsGruppe = sisteMeldingsGruppe
--        , ikkeVist = Ingen
--        }
--type alias MeldigsLoggInfo =
--    { sisteMeldingsGruppe : Maybe MeldingsGruppeIntern
--    , meldingsLogg : List MeldingsGruppeIntern
--    , ikkeVist : UskrevneMeldinger
--    }
--
--
--type MeldingsGruppeIntern
--    = SpørsmålGruppe_ (List Melding)
--    | SvarGruppe_ (List Melding)
--
--
--type MeldingsGruppe
--    = SpørsmålGruppe MeldingsGruppeMeldinger
--    | SvarGruppe MeldingsGruppeMeldinger
--
--
--type UskrevneMeldinger
--    = Ingen
--    | SkriverEnMelding Melding (List Melding)
--    | LeserMelding Melding (List Melding)
--
--
--type SkriveStatus
--    = Skriver
--    | SkriverIkke
