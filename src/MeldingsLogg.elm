module MeldingsLogg exposing
    ( FerdigAnimertMeldingsLogg
    , FerdigAnimertStatus(..)
    , MeldingsGruppe(..)
    , MeldingsLogg
    , MeldingsPlassering(..)
    , SkriveStatus(..)
    , ferdigAnimert
    , fullførMelding
    , init
    , leggTilSpørsmål
    , leggTilSvar
    , mapMeldingsGruppe
    , mapMeldingsGruppeMeldinger
    , nesteMeldingToString
    , skriveStatus
    , startÅSkrive
    , tilMeldingsLogg
    )

import Melding exposing (Melding)


type FerdigAnimertMeldingsLogg
    = FerdigAnimertMeldingsLogg (Maybe MeldingsGruppeIntern) (List MeldingsGruppeIntern)


type MeldingsLogg
    = MeldingsLogg MeldigsLoggInfo


type alias MeldigsLoggInfo =
    { sisteMeldingsGruppe : Maybe MeldingsGruppeIntern
    , meldingsLogg : List MeldingsGruppeIntern
    , ikkeVist : UskrevneMeldinger
    }


type MeldingsGruppeIntern
    = SpørsmålGruppe_ (List Melding)
    | SvarGruppe_ (List Melding)


type MeldingsGruppe
    = SpørsmålGruppe MeldingsGruppeMeldinger
    | SvarGruppe MeldingsGruppeMeldinger


type UskrevneMeldinger
    = Ingen
    | SkriverEnMelding Melding (List Melding)
    | LeserMelding Melding (List Melding)


type SkriveStatus
    = Skriver
    | SkriverIkke


init : MeldingsLogg
init =
    MeldingsLogg
        { sisteMeldingsGruppe = Nothing
        , meldingsLogg = []
        , ikkeVist = Ingen
        }



--
--meldinger : MeldingsLogg -> List Melding
--meldinger (MeldingsLogg { meldingsLogg }) =
--    List.concatMap meldingerFraMeldingsGruppe meldingsLogg
--
--
--meldingerFraMeldingsGruppe : MeldingsGruppe -> List Melding
--meldingerFraMeldingsGruppe meldingsGruppe =
--    case meldingsGruppe of
--        SpørsmålGruppe list ->
--            MeldingsGruppeMeldingerlist
--
--        SvarGruppe list ->
--            list


type MeldingsPlassering
    = SisteSpørsmålIMeldingsgruppe
    | IkkeSisteSpørsmål


type MeldingsGruppePlassering
    = SisteSpørsmålsgruppe
    | AnnenGruppe


mapMeldingsGruppe : (MeldingsGruppe -> a) -> MeldingsLogg -> List a
mapMeldingsGruppe function ((MeldingsLogg { meldingsLogg, sisteMeldingsGruppe }) as meldingsLogg_) =
    sisteMeldingsGruppe
        |> Maybe.map (fraMeldingsGruppeIntern SisteSpørsmålsgruppe (skriveStatus meldingsLogg_) >> List.singleton)
        |> Maybe.withDefault []
        |> List.append (List.map (fraMeldingsGruppeIntern AnnenGruppe (skriveStatus meldingsLogg_)) meldingsLogg)
        |> List.map function


fraMeldingsGruppeIntern : MeldingsGruppePlassering -> SkriveStatus -> MeldingsGruppeIntern -> MeldingsGruppe
fraMeldingsGruppeIntern meldingsGruppePlassering skriver meldingsGruppeIntern =
    case meldingsGruppeIntern of
        SpørsmålGruppe_ list ->
            case ( meldingsGruppePlassering, skriver ) of
                ( SisteSpørsmålsgruppe, Skriver ) ->
                    { meldingsLogg = list
                    , sisteMelding = Nothing
                    }
                        |> MeldingsGruppeMeldinger
                        |> SpørsmålGruppe

                _ ->
                    list
                        |> tilMeldingsGruppeMeldinger
                        |> SpørsmålGruppe

        SvarGruppe_ list ->
            { meldingsLogg = list
            , sisteMelding = Nothing
            }
                |> MeldingsGruppeMeldinger
                |> SvarGruppe


tilMeldingsGruppeMeldinger : List Melding -> MeldingsGruppeMeldinger
tilMeldingsGruppeMeldinger list =
    case List.reverse list of
        last :: rest ->
            { meldingsLogg = List.reverse rest
            , sisteMelding = Just last
            }
                |> MeldingsGruppeMeldinger

        [] ->
            { meldingsLogg = []
            , sisteMelding = Nothing
            }
                |> MeldingsGruppeMeldinger


type MeldingsGruppeMeldinger
    = MeldingsGruppeMeldinger
        { meldingsLogg : List Melding
        , sisteMelding : Maybe Melding
        }


mapMeldingsGruppeMeldinger : (MeldingsPlassering -> Melding -> a) -> MeldingsGruppeMeldinger -> List a
mapMeldingsGruppeMeldinger function (MeldingsGruppeMeldinger { meldingsLogg, sisteMelding }) =
    sisteMelding
        |> Maybe.map (function SisteSpørsmålIMeldingsgruppe >> List.singleton)
        |> Maybe.withDefault []
        |> List.append (List.map (function IkkeSisteSpørsmål) meldingsLogg)


nesteMeldingToString : MeldingsLogg -> Float
nesteMeldingToString (MeldingsLogg { ikkeVist }) =
    case ikkeVist of
        Ingen ->
            1.0

        SkriverEnMelding melding list ->
            let
                venteTid =
                    String.toFloat (String.fromInt (List.length (Melding.innhold melding))) |> Maybe.withDefault 1.0
            in
            if venteTid > 3.0 then
                3.0

            else
                venteTid

        LeserMelding melding list ->
            let
                venteTid =
                    String.toFloat (String.fromInt (List.length (Melding.innhold melding))) |> Maybe.withDefault 1.0
            in
            if venteTid > 3.0 then
                3.0

            else
                venteTid


skriveStatus : MeldingsLogg -> SkriveStatus
skriveStatus (MeldingsLogg { ikkeVist }) =
    case ikkeVist of
        Ingen ->
            SkriverIkke

        SkriverEnMelding _ _ ->
            Skriver

        LeserMelding _ _ ->
            SkriverIkke


leggTilSpørsmål : List Melding -> MeldingsLogg -> MeldingsLogg
leggTilSpørsmål nyeMeldinger (MeldingsLogg info) =
    MeldingsLogg
        { info
            | ikkeVist =
                case nyeMeldinger of
                    first :: rest ->
                        case info.ikkeVist of
                            Ingen ->
                                LeserMelding first rest

                            SkriverEnMelding melding list ->
                                SkriverEnMelding melding (list ++ nyeMeldinger)

                            LeserMelding melding list ->
                                LeserMelding melding (list ++ nyeMeldinger)

                    [] ->
                        Ingen
        }


leggTilSvar : Melding -> MeldingsLogg -> MeldingsLogg
leggTilSvar nyMelding (MeldingsLogg info) =
    case info.sisteMeldingsGruppe of
        Just (SpørsmålGruppe_ meldingsgruppe) ->
            MeldingsLogg
                { info
                    | meldingsLogg = info.meldingsLogg ++ [ SpørsmålGruppe_ meldingsgruppe ]
                    , sisteMeldingsGruppe = Just (SvarGruppe_ [ nyMelding ])
                }

        Just (SvarGruppe_ meldingsgruppe) ->
            MeldingsLogg { info | sisteMeldingsGruppe = Just (SvarGruppe_ (meldingsgruppe ++ [ nyMelding ])) }

        Nothing ->
            MeldingsLogg { info | sisteMeldingsGruppe = Just (SvarGruppe_ [ nyMelding ]) }


startÅSkrive : MeldingsLogg -> MeldingsLogg
startÅSkrive (MeldingsLogg info) =
    MeldingsLogg
        { info
            | ikkeVist =
                case info.ikkeVist of
                    Ingen ->
                        Ingen

                    SkriverEnMelding melding list ->
                        SkriverEnMelding melding list

                    LeserMelding melding list ->
                        SkriverEnMelding melding list
        }


fullførMelding : MeldingsLogg -> MeldingsLogg
fullførMelding (MeldingsLogg info) =
    case info.ikkeVist of
        Ingen ->
            MeldingsLogg info

        SkriverEnMelding melding list ->
            case info.sisteMeldingsGruppe of
                Just (SpørsmålGruppe_ meldingsgruppe) ->
                    MeldingsLogg
                        { info
                            | sisteMeldingsGruppe = Just (SpørsmålGruppe_ (meldingsgruppe ++ [ melding ]))
                            , ikkeVist = ikkeVistEtterFullførtMelding list
                        }

                Just (SvarGruppe_ meldingsgruppe) ->
                    MeldingsLogg
                        { info
                            | meldingsLogg = info.meldingsLogg ++ [ SvarGruppe_ meldingsgruppe ]
                            , sisteMeldingsGruppe = Just (SpørsmålGruppe_ [ melding ])
                            , ikkeVist = ikkeVistEtterFullførtMelding list
                        }

                Nothing ->
                    MeldingsLogg { info | sisteMeldingsGruppe = Just (SpørsmålGruppe_ [ melding ]), ikkeVist = ikkeVistEtterFullførtMelding list }

        LeserMelding melding list ->
            MeldingsLogg info


ikkeVistEtterFullførtMelding : List Melding -> UskrevneMeldinger
ikkeVistEtterFullførtMelding list =
    case list of
        first :: rest ->
            LeserMelding first rest

        [] ->
            Ingen


type FerdigAnimertStatus
    = FerdigAnimert FerdigAnimertMeldingsLogg
    | MeldingerGjenstår


ferdigAnimert : MeldingsLogg -> FerdigAnimertStatus
ferdigAnimert (MeldingsLogg info) =
    case info.ikkeVist of
        Ingen ->
            FerdigAnimert (FerdigAnimertMeldingsLogg info.sisteMeldingsGruppe info.meldingsLogg)

        SkriverEnMelding melding list ->
            MeldingerGjenstår

        LeserMelding melding list ->
            MeldingerGjenstår


tilMeldingsLogg : FerdigAnimertMeldingsLogg -> MeldingsLogg
tilMeldingsLogg (FerdigAnimertMeldingsLogg sisteMeldingsGruppe meldingslogg) =
    MeldingsLogg
        { meldingsLogg = meldingslogg
        , sisteMeldingsGruppe = sisteMeldingsGruppe
        , ikkeVist = Ingen
        }
